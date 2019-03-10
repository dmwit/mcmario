module Data.Partition
	( Partition
	, fromMap, unsafeFromSets
	, subset, subsets, sameSubset
	) where

import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

type Subset = Int

-- As an invariant, for all x :: Partition a, there is y :: Int such that:
-- (nub . map snd . toAscList . subset) x = [0..y]
data Partition a = Partition
	{ _subset :: Map a Subset
	, _elements :: IntMap (Set a)
	} deriving (Eq, Ord, Read, Show)

fromMap :: (Ord k, Ord v) => Map k v -> Partition k
fromMap m = Partition
	{ _subset = M.fromList kis
	, _elements = IM.fromListWith S.union [(i, S.singleton k) | (k,i) <- kis]
	}
	where
	subsetIndex = buildSubsetIndex 0 M.empty (M.toAscList m)
	buildSubsetIndex i si [] = si
	buildSubsetIndex i si ((k,v):kvs) | M.member v si = buildSubsetIndex i si kvs
	                                  | otherwise     = buildSubsetIndex (i+1) (M.insert v i si) kvs
	kis = [(k, subsetIndex M.! v) | (k, v) <- M.toList m]

-- Just for convenience. In a non-MCMario setting I would never include this in
-- the API.

-- | This assumes that all the sets given to it are disjoint (and this
-- assumption is not checked).
unsafeFromSets :: Ord a => [Set a] -> Partition a
unsafeFromSets ss = fromMap (M.fromList [(a, s) | s <- ss, a <- S.toList s])

-- TODO: Can we get away with *just* `sameSubset`? If so, how much simpler can
-- the code go? (e.g. perhaps `_elements` can disappear, and `fromMap` or
-- `unsafeFromSets` can get simpler)
subset :: Ord a => Partition a -> a -> Set a
subset p a = fromMaybe (S.singleton a) (M.lookup a (_subset p) >>= flip IM.lookup (_elements p))

-- | Explicitly enumerate the sets that make up the partition.
subsets :: Partition a -> [Set a]
subsets = IM.elems . _elements

-- TODO: make x == y a fast path?
-- | @sameSubset p x y = subset p x == subset p y@, but more efficient
sameSubset :: Ord a => Partition a -> a -> a -> Bool
sameSubset p x y = case M.lookup x (_subset p) of
	Nothing -> x == y
	Just s  -> maybe False (s==) (M.lookup y (_subset p))
