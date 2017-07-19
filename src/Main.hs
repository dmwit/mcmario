{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Csv
	( DefaultOrdered
	, EncodeOptions
	, FromField
	, FromNamedRecord
	, ToField
	, ToNamedRecord
	)
import Data.Default
import Data.Foldable
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Time
import MCMario.GameDB
import MCMario.RatingDB
import System.Environment
import System.Exit
import System.IO
import System.Posix.Files
import Snap
-- TODO: snap-extras brings along a pretty enormous collection of dependencies.
-- Are they really worth the extra ten minutes build time just for writeJSON,
-- which is after all a two-line function?
import Snap.Extras.JSON
import Snap.Util.FileServe
import Text.Read

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.Csv as CSV
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

instance ToNamedRecord GameRecord where
	toNamedRecord r = CSV.namedRecord
		[ fromString "winner-name"  CSV..= name  (winner r)
		, fromString "winner-level" CSV..= level (winner r)
		, fromString "winner-speed" CSV..= speed (winner r)
		, fromString "loser-name"   CSV..= name  (loser  r)
		, fromString "loser-level"  CSV..= level (loser  r)
		, fromString "loser-speed"  CSV..= speed (loser  r)
		, fromString "time"         CSV..= date r
		]

instance DefaultOrdered GameRecord where
	headerOrder _ = CSV.header
		[ fromString "winner-name"
		, fromString "winner-level"
		, fromString "winner-speed"
		, fromString "loser-name"
		, fromString "loser-level"
		, fromString "loser-speed"
		, fromString "time"
		]

instance FromNamedRecord GameRecord where
	parseNamedRecord m = go where
		go = liftA3 GameRecord
			(parseSettings "winner")
			(parseSettings "loser")
			(m CSV..: fromString "time")
		parseSettings prefix = liftA3 PlayerSettings
			(m CSV..: fromString (prefix <> "-name"))
			(m CSV..: fromString (prefix <> "-level"))
			(m CSV..: fromString (prefix <> "-speed"))

instance ToField Speed where
	toField High   = fromString "HI"
	toField Medium = fromString "MED"
	toField Low    = fromString "LOW"

instance FromField Speed where
	parseField bs
		| bs == fromString "HI"  = pure High
		| bs == fromString "MED" = pure Medium
		| bs == fromString "LOW" = pure Low
		| otherwise = fail "expected HI, MED, or LOW"

instance ToField UTCTime where toField = fromString . show

instance FromField UTCTime where
	parseField bs = case readMaybe (BS.unpack bs) of
		Just t  -> pure t
		Nothing -> fail "expected a date"

instance Default EncodeOptions where def = CSV.defaultEncodeOptions

load :: FilePath -> IO GameDB
load filename = do
	bs <- LBS.readFile filename
	case CSV.decodeByName bs of
		Left err -> die err
		Right (_, gs) -> return (foldr addGame def gs)

save :: FilePath -> GameDB -> IO ()
save filename gdb = do
	LBS.writeFile
		(filename <> ".new")
		(CSV.encodeByNameWith def header (listGames gdb))
	rename (filename <> ".new") filename
	where
	header = CSV.headerOrder (undefined :: GameRecord)

defaultFilename :: FilePath
defaultFilename = "games.csv"

data Context = Context
	{ filename :: FilePath
	, gameDB :: TVar GameDB
	, ratingDB :: TVar RatingDB
	, ratingDBDirty :: TVar Bool
	, diskDirty :: TVar Bool
	}

saveThread :: Context -> IO ()
saveThread ctxt = forever $ do
	gdb <- atomically $ do
		dirty <- readTVar (diskDirty ctxt)
		guard dirty
		writeTVar (diskDirty ctxt) False
		readTVar (gameDB ctxt)
	save (filename ctxt) gdb

-- TODO: restart the calculations in the middle if the dirty bit gets set
ratingsUpdateThread :: Context -> IO ()
ratingsUpdateThread ctxt = forever . atomically $ do
	dirty <- readTVar (ratingDBDirty ctxt)
	guard dirty
	gdb <- readTVar (gameDB ctxt)
	writeTVar (ratingDB ctxt) $!! inferRatings gdb
	writeTVar (ratingDBDirty ctxt) False

readTVarSnap :: MonadIO m => TVar a -> m a
readTVarSnap = liftIO . readTVarIO

listPlayersJSON :: Context -> Snap ()
listPlayersJSON ctxt = ifTop $
	writeJSON . listPlayers =<< readTVarSnap (gameDB ctxt)

requireUtf8Param :: MonadSnap m => BS.ByteString -> m Text
requireUtf8Param label = do
	bs <- getParam label
	case T.decodeUtf8' <$> bs of
		Just (Right text) -> return text
		_ -> pass

requireJSONParam :: (MonadSnap m, FromJSON a) => BS.ByteString -> m a
requireJSONParam label = do
	bs <- getParam label
	case bs >>= JSON.decodeStrict of
		Just v -> return v
		_ -> pass

JSON.deriveJSON JSON.defaultOptions ''Confidence
JSON.deriveJSON JSON.defaultOptions ''Speed
JSON.deriveJSON JSON.defaultOptions ''PlayerSettings
JSON.deriveJSON JSON.defaultOptions ''GameRecord
JSON.deriveJSON JSON.defaultOptions ''Matchup

matchupJSON :: Context -> Snap ()
matchupJSON ctxt = do
	left  <- requireUtf8Param (fromString "left" )
	right <- requireUtf8Param (fromString "right")
	(gdb, rdb) <- liftIO . atomically $ do
		gdb <- readTVar (gameDB ctxt)
		rdb <- readTVar (ratingDB ctxt)
		return (gdb, rdb)
	writeJSON (matchup gdb rdb left right)

addGamePost :: Context -> Snap ()
addGamePost ctxt = do
	winner <- requireJSONParam (fromString "winner")
	loser  <- requireJSONParam (fromString "loser" )
	now <- liftIO getCurrentTime
	liftIO . atomically $ do
		gdb <- readTVar (gameDB ctxt)
		writeTVar (gameDB ctxt) (addGame (GameRecord winner loser now) gdb)
		writeTVar (ratingDBDirty ctxt) True
		writeTVar (diskDirty     ctxt) True

main = do
	args <- getArgs
	fn <- case args of
		[fn] -> return fn
		[] -> return defaultFilename
		_ -> do
			progName <- getProgName
			die $  "USAGE: " <> progName <> " [FILE]\n"
			    <> "Manage a database of Dr. Mario games and get suggestions for handicapped games\n"
			    <> "\n"
			    <> "FILE defaults to " <> defaultFilename
	fPort <- do
		s <- getEnv "MCMARIO_PORT"
		case (s, readMaybe s) of
			("", _) -> return id
			(_, Just p) | p > 0 -> return (setPort p)
			_ -> die "MCMARIO_PORT must be a positive number"
	gdb <- load fn `catch` \e -> do
		let e' :: IOException
		    e' = e
		hPutStrLn stderr $ "WARNING: Could not open " <> fn <> "; using an empty game database"
		return def
	ctxt <- Context fn
		<$> newTVarIO gdb
		<*> newTVarIO def
		<*> newTVarIO True
		<*> newTVarIO False
	forkIO (saveThread ctxt)
	forkIO (ratingsUpdateThread ctxt)
	httpServe (fPort mempty) . asum $
		[ method GET . route $
			[ (fromString "players", listPlayersJSON ctxt)
			, (fromString "matchup/:left/:right", matchupJSON ctxt)
			]
		, method POST . route $
			[ (fromString "game", addGamePost ctxt)
			]
		, serveDirectory "static"
		]
