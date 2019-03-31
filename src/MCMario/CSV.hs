module MCMario.CSV (loadGames, saveGames, loadRatings, saveRatings) where

import Control.Applicative
import Control.Exception
import Data.Csv
	( DefaultOrdered
	, EncodeOptions
	, FromField
	, FromNamedRecord
	, ToField
	, ToNamedRecord
	)
import Data.Default
import Data.Fixed
import Data.Monoid
import Data.Ratio
import Data.Set (Set)
import Data.String
import Data.Time
import Data.Vector (Vector)
import MCMario.GameDB
import MCMario.Model
import MCMario.RatingDB
import System.Exit
import System.Posix.Files
import System.IO.Error
import Text.Read

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import qualified Data.Map as M
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

missingFileIsDef :: Default a => IOError -> IO (Either String a)
missingFileIsDef e
	| isDoesNotExistError e = return (Right def)
	| otherwise = throwIO e

loadGames :: FilePath -> IO (Either String GameDB)
loadGames filename = flip catch missingFileIsDef $ do
	bs <- LBS.readFile filename
	case CSV.decodeByName bs of
		Left err -> return (Left err)
		Right (_, gs) -> return (Right (foldr addGame def gs))

saveGames :: FilePath -> GameDB -> IO ()
saveGames filename gdb = do
	LBS.writeFile
		(filename <> ".new")
		(CSV.encodeByNameWith def header (listGames gdb))
	rename (filename <> ".new") filename
	where
	header = CSV.headerOrder (undefined :: GameRecord)

newtype RatingEntry = RatingEntry (Name, Rate)
	deriving (Eq, Ord, Read, Show)

instance ToNamedRecord RatingEntry where
	toNamedRecord (RatingEntry (n, r)) = CSV.namedRecord
		[ fromString "name"   CSV..= n
		, fromString "rating" CSV..= r
		]

instance DefaultOrdered RatingEntry where
	headerOrder _ = CSV.header
		[ fromString "name"
		, fromString "rating"
		]

instance FromNamedRecord RatingEntry where
	parseNamedRecord m = pure (\n r -> RatingEntry (n,r))
		<*> (m CSV..: fromString "name")
		<*> (m CSV..: fromString "rating")

instance HasResolution a => ToField (Fixed a) where toField = fromString . show

instance HasResolution a => FromField (Fixed a) where
	parseField bs = case readMaybe (BS.unpack bs) of
		Just t -> pure t
		Nothing -> fail "expected a rating"

loadRatings :: FilePath -> IO (Either String RatingDB)
loadRatings filename = flip catch missingFileIsDef $ do
	bs <- LBS.readFile filename
	case CSV.decodeByName bs of
		Left err -> return (Left err)
		Right (_, rs) -> return . Right . M.fromList . map unRatingEntry . V.toList $ rs
	where
	unRatingEntry (RatingEntry (n, r)) = (n, max epsilon r)

saveRatings :: FilePath -> RatingDB -> IO ()
saveRatings filename rdb = do
	LBS.writeFile
		(filename <> ".new")
		(CSV.encodeByNameWith def header (map RatingEntry $ M.toList rdb))
	rename (filename <> ".new") filename
	where
	header = CSV.headerOrder (undefined :: RatingEntry)
