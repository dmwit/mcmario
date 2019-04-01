{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Csv (FromField, parseField, runParser)
import Data.Default
import Data.Either
import Data.Foldable
import Data.List
import Data.Monoid
import Data.Ratio
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Word
import MCMario.CSV
import MCMario.GameDB
import MCMario.RatingDB
import MCMario.STM
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import Snap
-- TODO: snap-extras brings along a pretty enormous collection of dependencies.
-- Are they really worth the extra ten minutes build time just for writeJSON,
-- which is after all a two-line function?
import Snap.Extras.JSON
import Snap.Util.FileServe
import Text.Read

import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

defaultGamesFile :: FilePath
defaultGamesFile = "games.csv"

defaultRatingsFile :: FilePath
defaultRatingsFile = "ratings.csv"

data Context = Context
	{ gamesFile :: TMVar FilePath
	, ratingsFile :: TMVar FilePath
	, queuedUpdates :: TVar [Name]
	, currentUpdate :: TVar (Maybe (Name, Word))
	}

-- How many times to try improving the rating database before writing the
-- result and checking for an updated game database. Fewer is better because it
-- means recommendations adjust to new data more quickly; more is better
-- because it means we spend less time doing disk access per improvement.
iterationsPerSTMUpdate :: Word
iterationsPerSTMUpdate = 50

-- Declare success even if the ratings database is still changing after this
-- many attempts to improve it.
maxIterations :: Word
maxIterations = 10000

loadCtxt :: MonadIO m =>
	(FilePath -> IO (Either String a)) ->
	TMVar FilePath -> m (Either String a)
loadCtxt loadDB fileLock = liftIO $ do
	file <- atomically $ takeTMVar fileLock
	mres <- loadDB file
	atomically $ putTMVar fileLock file
	return mres

loadGamesCtxt :: MonadIO m => Context -> m (Either String GameDB)
loadGamesCtxt = loadCtxt loadGames . gamesFile

loadGamesCtxt' :: MonadIO m => Context -> m GameDB
loadGamesCtxt' ctxt = fromRight def <$> loadGamesCtxt ctxt

loadRatingsCtxt :: MonadIO m => Context -> m (Either String RatingDB)
loadRatingsCtxt = loadCtxt loadRatings . ratingsFile

loadRatingsCtxt' :: MonadIO m => Context -> m RatingDB
loadRatingsCtxt' ctxt = fromRight def <$> loadRatingsCtxt ctxt

saveCtxt :: (Eq a, MonadIO m) =>
	(FilePath -> IO (Either String a)) -> (FilePath -> a -> IO ()) ->
	TMVar FilePath -> a -> a -> m (Maybe (Either String a))
saveCtxt loadDB saveDB fileLock db db' = liftIO $ do
	file <- atomically $ takeTMVar fileLock
	dbFromDisk <- loadDB file
	let asExpected = dbFromDisk == Right db
	when asExpected (saveDB file db')
	atomically $ putTMVar fileLock file
	return $ if asExpected then Nothing else Just dbFromDisk

saveGamesCtxt :: MonadIO m => Context -> GameDB -> GameDB -> m (Maybe (Either String GameDB))
saveGamesCtxt = saveCtxt loadGames saveGames . gamesFile

saveRatingsCtxt :: MonadIO m => Context -> RatingDB -> RatingDB -> m (Maybe (Either String RatingDB))
saveRatingsCtxt = saveCtxt loadRatings saveRatings . ratingsFile

reportBlocker :: String -> Either String a -> IO ()
reportBlocker dbType (Left err) = putStrLn
	$  "WARNING: corrupted " ++ dbType ++ " database is preventing ratings updates; "
	++ "will try rereading it in 10 seconds.\n\t"
	++ err
reportBlocker _ _ = return ()

retrieveComponent :: GameDB -> Name -> IO (Maybe Component)
retrieveComponent gdb name = case filter (S.member name) (components gdb) of
	[component] -> return (Just component)
	[] -> do
		putStrLn
			$ "WARNING: ignoring requested update for "
			++ T.unpack name
			++ "; no component contains them."
		return Nothing
	cs -> do
		hPutStrLn stderr
			$ "The impossible happened! "
			++ T.unpack name
			++ " is in multiple components:"
		mapM_ (\c -> hPutStrLn stderr ("\t" ++ show c)) cs
		hPutStrLn stderr $ "(Ignoring ratings update request for this name as a result.)"
		return Nothing

componentSelectionThread :: Context -> IO ()
componentSelectionThread ctxt = forever $ do
	name <- atomically $ do
		Nothing <- readTVar (currentUpdate ctxt)
		name:names <- readTVar (queuedUpdates ctxt)
		writeTVar (queuedUpdates ctxt) names
		return name
	mgdb <- loadGamesCtxt ctxt
	reportBlocker "game" mgdb
	case mgdb of
		Right gdb -> do
			mComponent <- retrieveComponent gdb name
			forM_ mComponent $ \component -> atomically $ do
				modifyTVar (queuedUpdates ctxt) (filter (`S.notMember` component))
				writeTVar (currentUpdate ctxt) (Just (name, maxIterations))
		Left err -> do
			atomically $ modifyTVar (queuedUpdates ctxt) (name:)
			threadDelay 10000000

ratingsUpdateThread :: Context -> IO ()
ratingsUpdateThread ctxt = forever $ do
	(name, iterations) <- atomically $ do
		Just update <- readTVar (currentUpdate ctxt)
		return update
	mgdb <- loadGamesCtxt ctxt
	reportBlocker "game" mgdb
	mrdb <- loadRatingsCtxt ctxt
	reportBlocker "ratings" mrdb
	case liftA2 (,) mgdb mrdb of
		Left _ -> threadDelay 10000000
		Right (gdb, rdb) -> do
			mComponent <- retrieveComponent gdb name
			forM_ mComponent $ \component -> do
				let rdb' = genericIndex (improveComponentRatings gdb rdb component)
				                        (min iterations iterationsPerSTMUpdate)
				evaluate (rnf rdb')
				merr <- saveRatingsCtxt ctxt rdb rdb'
				case merr of
					Just _ -> putStrLn "WARNING: Ratings database modified while computing a ratings update. Discarding the update."
					Nothing -> atomically . writeTVar (currentUpdate ctxt) $
						if iterations <= iterationsPerSTMUpdate
						then Nothing
						else Just (name, iterations-iterationsPerSTMUpdate)

listPlayersJSON :: Context -> Snap ()
listPlayersJSON ctxt = writeJSON . listPlayers =<< loadGamesCtxt' ctxt

requireFieldParam :: (MonadSnap m, FromField a) => BS.ByteString -> m a
requireFieldParam label = do
	bs <- getParam label
	case runParser . parseField <$> bs of
		Just (Right v) -> return v
		_ -> pass

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
JSON.deriveJSON JSON.defaultOptions ''Winner
JSON.deriveJSON JSON.defaultOptions ''GameRecord
JSON.deriveJSON JSON.defaultOptions ''Matchup

matchupJSON :: Context -> Snap ()
matchupJSON ctxt = do
	left  <- requireUtf8Param (fromString "left" )
	right <- requireUtf8Param (fromString "right")
	gdb <- loadGamesCtxt' ctxt
	rdb <- loadRatingsCtxt' ctxt
	writeJSON (matchup gdb rdb (S.singleton left) (S.singleton right))

addGamePost :: Context -> Snap ()
addGamePost ctxt = do
	blue   <- requireUtf8Param (fromString   "blue")
	orange <- requireUtf8Param (fromString "orange")
	winner <- requireFieldParam (fromString "winner")
	num    <- requireJSONParam (fromString "orange-multiplier")
	den    <- requireJSONParam (fromString   "blue-multiplier")
	now <- liftIO getCurrentTime
	gdb <- loadGamesCtxt' ctxt
	let gdb' = addGame (GameRecord (S.singleton blue) (S.singleton orange) (num % den) winner now) gdb
	failure <- saveGamesCtxt ctxt gdb gdb'
	case failure of
		Nothing -> atomically $ modifyTVar (queuedUpdates ctxt) (++[blue,orange])
		Just _ -> modifyResponse (setResponseCode 500)

debug :: Context -> Snap ()
debug ctxt = do
	modifyResponse (setContentType (fromString "text/plain"))
	gdb <- loadGamesCtxt ctxt
	rdb <- loadRatingsCtxt ctxt
	(gameFile, ratingFile, queued, current) <- atomically $ do
		gameFile   <- readTMVar (gamesFile     ctxt)
		ratingFile <- readTMVar (ratingsFile   ctxt)
		queued     <- readTVar  (queuedUpdates ctxt)
		current    <- readTVar  (currentUpdate ctxt)
		return (gameFile, ratingFile, queued, current)
	writeString $  "Storing games in " <> show gameFile <> "\n"
	writeString $  "Storing ratings in " <> show ratingFile <> "\n"
	writeString $  "The following players have played since their components' ratings were last updated:\n"
	            <> unlines ["\t" <> show name | name <- queued]
	writeString $ case current of
		Nothing -> "The rating database is not currently being updated.\n"
		Just (name, iterations)
			-> "The rating database is currently updating the ratings for " <> show name <> "'s component.\n"
			<> "\t" <> show iterations <> " update steps remain.\n"
	writeString $  "\t(currently configured to take " <> show iterationsPerSTMUpdate
	            <> " improvement steps per write, up to " <> show maxIterations <> " total)\n"
	writeString $  "Complete rating database dump:\n" <> show rdb <> "\n"
	writeString $  "Complete game database dump:\n" <> show gdb <> "\n"
	where writeString = writeText . fromString

serveCSV :: TMVar FilePath -> Snap ()
serveCSV mfile = do
	modifyResponse (setContentType (fromString "text/csv"))
	file <- atomically $ readTMVar mfile
	sendFile file

gamesCSV :: Context -> Snap ()
gamesCSV = serveCSV . gamesFile

ratingsCSV :: Context -> Snap ()
ratingsCSV = serveCSV . ratingsFile

main :: IO ()
main = do
	args <- getArgs
	(gamesfn, ratingsfn) <- case args of
		[] -> return (defaultGamesFile, defaultRatingsFile)
		[gamesfn] -> return (gamesfn, defaultRatingsFile)
		[gamesfn, ratingsfn] -> return (gamesfn, ratingsfn)
		_ -> do
			progName <- getProgName
			die $  "USAGE: " <> progName <> " [FILE [FILE]]\n"
			    <> "Manage a database of Rocket League games and get suggestions for handicapped games\n"
			    <> "\n"
			    <> "The first FILE stores the games and defaults to " <> defaultGamesFile <> ".\n"
			    <> "The second FILE stores the ratings and defaults to " <> defaultRatingsFile <> "."
	fPort <- do
		s <- catch (getEnv "MCMARIO_PORT")
		           (\e -> if isDoesNotExistError e then return "" else throwIO e)
		case (s, readMaybe s) of
			("", _) -> return id
			(_, Just p) | p > 0 -> return (setPort p)
			_ -> die "MCMARIO_PORT must be a positive number"
	ctxt <- Context
		<$> newTMVarIO gamesfn
		<*> newTMVarIO ratingsfn
		<*> newTVarIO []
		<*> newTVarIO Nothing
	forkIO (componentSelectionThread ctxt)
	forkIO (ratingsUpdateThread ctxt)
	httpServe (fPort mempty) . asum $
		[ method GET . route $
			[ (fromString "players", ifTop $ listPlayersJSON ctxt)
			, (fromString "matchup/:left/:right", ifTop $ matchupJSON ctxt)
			, (fromString "debug.txt", ifTop $ debug ctxt)
			-- the endpoints for the next two are always games.csv and
			-- ratings.csv, no matter what actual database filename is being
			-- used
			, (fromString "games.csv", ifTop $ gamesCSV ctxt)
			, (fromString "ratings.csv", ifTop $ ratingsCSV ctxt)
			]
		, method POST . route $
			[ (fromString "game", ifTop $ addGamePost ctxt)
			]
		, serveDirectory "static"
		]
