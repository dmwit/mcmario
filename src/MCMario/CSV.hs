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
import Data.Ratio
import Data.Set (Set)
import Data.String
import Data.Time
import Data.Vector (Vector)
import MCMario.GameDB
import MCMario.RatingDB
import System.Exit
import System.Posix.Files
import Text.Read

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import qualified Data.Set as S
import qualified Data.Vector as V

instance ToNamedRecord GameRecord where
	toNamedRecord r = CSV.namedRecord
		[ fromString "blue-names"        CSV..= blue r
		, fromString "blue-multiplier"   CSV..= denominator (handicap r)
		, fromString "orange-names"      CSV..= orange r
		, fromString "orange-multiplier" CSV..= numerator (handicap r)
		, fromString "winner"            CSV..= winner r
		, fromString "time"              CSV..= date r
		]

instance DefaultOrdered GameRecord where
	headerOrder _ = CSV.header
		[ fromString "blue-names"
		, fromString "blue-multiplier"
		, fromString "orange-names"
		, fromString "orange-multiplier"
		, fromString "winner"
		, fromString "time"
		]

instance FromNamedRecord GameRecord where
	parseNamedRecord m = pure GameRecord
		<*> (m CSV..: fromString   "blue-names")
		<*> (m CSV..: fromString "orange-names")
		<*> liftA2 (%) (m CSV..: fromString "orange-multiplier")
		               (m CSV..: fromString   "blue-multiplier")
		<*> (m CSV..: fromString "winner")
		<*> (m CSV..: fromString "time")

instance ToField Winner where
	toField Blue   = fromString "blue"
	toField Tie    = fromString "tie"
	toField Orange = fromString "orange"

instance FromField Winner where
	parseField bs
		| bs == fromString "blue"   = pure Blue
		| bs == fromString "tie"    = pure Tie
		| bs == fromString "orange" = pure Orange
		| otherwise = fail "expected blue, tie, or orange"

instance ToField UTCTime where toField = fromString . show

instance FromField UTCTime where
	parseField bs = case readMaybe (BS.unpack bs) of
		Just t  -> pure t
		Nothing -> fail "expected a date"

instance ToField a => ToField (Set a) where
	toField s = LBS.toStrict $ CSV.encode [S.toList s]

instance (Ord a, FromField a) => FromField (Set a) where
	parseField bs = case CSV.decode CSV.NoHeader $ LBS.fromChunks [bs] of
		Right v -> case V.length v of
			1 -> pure . S.fromList $ v V.! 0
			n -> fail $ "expected exactly one row of CSV, found " ++ show n
		Left err -> fail err

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
