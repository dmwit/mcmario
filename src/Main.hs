module Main where

import Control.Applicative
import Data.Csv
import Data.Default
import Data.Monoid
import Data.String
import Data.Time
import MCMario.GameDB
import MCMario.RatingDB
import System.Environment
import System.Exit
import System.Posix.Files
import Text.Read
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

instance ToNamedRecord GameRecord where
	toNamedRecord r = namedRecord
		[ fromString "winner-name"  .= name  (winner r)
		, fromString "winner-level" .= level (winner r)
		, fromString "winner-speed" .= speed (winner r)
		, fromString "loser-name"   .= name  (loser  r)
		, fromString "loser-level"  .= level (loser  r)
		, fromString "loser-speed"  .= speed (loser  r)
		, fromString "time"         .= date r
		]

instance DefaultOrdered GameRecord where
	headerOrder _ = header
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
			(m .: fromString "time")
		parseSettings prefix = liftA3 PlayerSettings
			(m .: fromString (prefix <> "-name"))
			(m .: fromString (prefix <> "-level"))
			(m .: fromString (prefix <> "-speed"))

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

instance Default EncodeOptions where def = defaultEncodeOptions

load :: FilePath -> IO GameDB
load filename = do
	bs <- LBS.readFile filename
	case decodeByName bs of
		Left err -> die err
		Right (_, gs) -> return (foldr addGame def gs)

save :: FilePath -> GameDB -> IO ()
save filename gdb = do
	LBS.writeFile
		(filename <> ".new")
		(encodeByNameWith def (headerOrder GameRecord{}) (listGames gdb))
	rename (filename <> ".new") filename

main = do
	args <- getArgs
	case args of
		[filename, "show"] -> load filename >>= print
		[filename, "init"] -> save filename def
		[filename, "add", n1_, l1_, s1_, n2_, l2_, s2_] -> do
			l1 <- readIO l1_
			s1 <- readIO s1_
			l2 <- readIO l2_
			s2 <- readIO s2_
			now <- getCurrentTime
			gdb <- load filename
			let n1 = T.pack n1_
			    n2 = T.pack n2_
			    r = GameRecord (PlayerSettings n1 l1 s1) (PlayerSettings n2 l2 s2) now
			save filename (addGame r gdb)
		[filename, "suggest", n1_, n2_] -> do
			gdb <- load filename
			let n1 = T.pack n1_
			    n2 = T.pack n2_
			    rdb = inferRatings gdb
			print (matchup gdb rdb n1 n2)
		_ -> do
			progName <- getProgName
			putStrLn $ "USAGE: " <> progName <> " FILE CMD [ARGS]"
			putStrLn "Manage a database of Dr. Mario games and get suggestions for handicapped games"
			putStrLn ""
			putStrLn "Commands are:"
			putStrLn ""
			putStrLn "\tinit      Create a new empty database"
			putStrLn "\tshow      Print the contents of the database in a raw Haskell-y format"
			putStrLn "\tadd PLAYER LEVEL SPEED PLAYER LEVEL SPEED"
			putStrLn "\t          Add a game to the database. Winner comes first"
			putStrLn "\tsuggest PLAYER PLAYER"
			putStrLn "\t          Suggest settings for a match"
