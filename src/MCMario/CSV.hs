module MCMario.CSV (load, save) where

import Control.Applicative
import Data.Csv
	( DefaultOrdered
	, EncodeOptions
	, FromField
	, FromNamedRecord
	, ToField
	, ToNamedRecord
	)
import Data.Default
import Data.Monoid
import Data.String
import Data.Time
import MCMario.GameDB
import MCMario.RatingDB
import System.Exit
import System.Posix.Files
import Text.Read

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV

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
