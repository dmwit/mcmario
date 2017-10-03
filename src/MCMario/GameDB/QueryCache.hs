module MCMario.GameDB.QueryCache
	( -- | A cache for information that could, in theory, be computed from a
	  -- 'GameDB', but which can be more cheaply incrementally updated as
	  -- 'GameRecord's are added.
	  QueryCache
	, gdb
	, fromGameDB
	) where

import Data.Map (Map)
import Data.Monoid
import Data.MonoidMap (MMap)
import Data.Partition (Partition)
import MCMario.GameDB (Name, Speed(..), PlayerSettings(..), GameRecord(..), GameDB)
import qualified Data.Map as M
import qualified Data.MonoidMap as MM
import qualified Data.Partition as P
import qualified MCMario.GameDB as GDB

data QueryCache = QueryCache
	{ gdb :: GameDB
	, playerStats :: MMap Name PlayerStats
	, components :: Partition Name
	} deriving (Eq, Show)

fromGameDB :: GameDB -> QueryCache
fromGameDB gdb = QueryCache
	{ gdb = gdb
	, playerStats = foldMap fromGameRecord (GDB.listGames gdb)
	, components = P.unsafeFromSets (GDB.components gdb)
	}

addGame :: GameRecord -> QueryCache -> QueryCache
addGame g qc = QueryCache
	{ gdb = gdb'
	, playerStats = mappend (fromGameRecord g) (playerStats qc)
	, components = if P.sameSubset (components qc) (name (winner g)) (name (loser g))
	               then components qc
	               else P.unsafeFromSets (GDB.components gdb')
	}
	where
	gdb' = GDB.addGame g (gdb qc)

data PlayerStats = PlayerStats
	{ games  :: Sum Int
	, speeds :: MMap Speed (Sum Int)
	, levels :: MMap Integer (Sum Int)
	} deriving (Eq, Ord, Show)

instance Monoid PlayerStats where
	mempty = PlayerStats
		{ games  = mempty
		, speeds = mempty
		, levels = mempty
		}
	mappend s1 s2 = PlayerStats
		{ games  = games  s1 `mappend` games  s2
		, speeds = speeds s1 `mappend` speeds s2
		, levels = levels s1 `mappend` levels s2
		}

fromGameRecord :: GameRecord -> MMap Name PlayerStats
fromGameRecord g = fromPlayerSettings (winner g) `mappend` fromPlayerSettings (loser g)

fromPlayerSettings :: PlayerSettings -> MMap Name PlayerStats
fromPlayerSettings ps = MM.singleton (name ps) PlayerStats
	{ games  = 1
	, speeds = MM.singleton (speed ps) 1
	, levels = MM.singleton (level ps) 1
	}
