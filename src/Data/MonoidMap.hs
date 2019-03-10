module Data.MonoidMap
	( MMap
	, singleton
	, fromList
	, fromDistinctAscList
	, (!)
	, toList
	) where

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as M

-- | Behaves like a function but has the performance characteristics of a 'Map'.
newtype MMap k v = MMap { unMMap :: Map k v }

instance (Ord k, Monoid v, Eq v) => Eq (MMap k v) where
	MMap m == MMap m' = and $ M.mergeWithKey
		(\k v v' -> if v == v' then Nothing else Just False)
		((mempty==) <$>)
		((mempty==) <$>)
		m
		m'

instance (Ord k, Monoid v, Ord v) => Ord (MMap k v) where
	compare (MMap m) (MMap m') = fold $ M.mergeWithKey
		(\k v v' -> Just (compare v v'))
		((`compare`mempty) <$>)
		((mempty`compare`) <$>)
		m
		m'

instance (Ord k, Eq v, Monoid v, Show k, Show v) => Show (MMap k v) where
	showsPrec n = showsPrec n . unMMap . prune

instance (Ord k, Monoid v) => Monoid (MMap k v) where
	mempty = MMap M.empty
	mappend (MMap m) (MMap m') = MMap (M.unionWith mappend m m')

singleton :: k -> v -> MMap k v
singleton k v = MMap (M.singleton k v)

fromList :: Ord k => [(k, v)] -> MMap k v
fromList = MMap . M.fromList

fromDistinctAscList :: [(k, v)] -> MMap k v
fromDistinctAscList = MMap . M.fromDistinctAscList

(!) :: (Ord k, Monoid v) => MMap k v -> k -> v
MMap m ! k = M.findWithDefault mempty k m

toList :: (Monoid v, Eq v) => MMap k v -> [(k, v)]
toList = M.toList . unMMap . prune

-- | Semantically the identity, but operationally deletes keys that map to
-- 'mempty'.
prune :: (Monoid v, Eq v) => MMap k v -> MMap k v
prune (MMap m) = MMap (M.filter (mempty/=) m)
