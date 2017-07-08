module Main where

import Control.Applicative
import Data.Fixed
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Time
import Math.Polynomial
import System.Environment
import qualified Data.Map as M
import qualified Data.Text as T

infix  8 %^
infixl 7 %*%
infix  7  *%
infixl 6 %+%
infixl 6 %-%
(%^ ) :: (Eq a, Num a) => Poly a -> Integer -> Poly a
(%*%) :: (Eq a, Num a) => Poly a -> Poly a  -> Poly a
( *%) :: (Eq a, Num a) =>      a -> Poly a  -> Poly a
(%+%) :: (Eq a, Num a) => Poly a -> Poly a  -> Poly a
(%-%) :: (Eq a, Num a) => Poly a -> Poly a  -> Poly a
(%^ ) = powPoly
(%*%) = multPoly
(*% ) = scalePoly
(%+%) = addPoly
p1 %-% p2 = p1 %+% negatePoly p2

-- links of interest:
--
-- https://stats.stackexchange.com/q/264861
-- https://en.wikipedia.org/wiki/Gamma_distribution
-- https://en.wikipedia.org/wiki/Beta_distribution
-- https://en.wikipedia.org/wiki/Binomial_distribution

-- | The CDF of the Beta distribution with parameters @n1@ and @n2@ is @beta n1 n2@.
beta :: (Eq a, Fractional a) => Integer -> Integer -> Poly a
beta n1 n2 = orderings *% poly where
	[lo, hi] = sort [n1, n2] -- do fewer multiplications
	orderings = fromInteger $ product [hi .. lo + hi - 1] `div` product [2 .. lo - 1]
	poly = polyIntegral $ x %^ (n1-1) %*% (one %-% x) %^ (n2-1)

-- | If an event happens with probability @p@, then that event happens at least
-- three times out of five with probability @6p^5 - 15p^4 + 10p^3@.
threeOutOfFive :: (Eq a, Num a) => Poly a -> Poly a
threeOutOfFive f = f %^ 3 %*% (6 *% f %^ 2 %-% 15 *% f %+% constPoly 10)

-- | There are only a couple hundred polynomials we ever actually use. No sense
-- recomputing them a lot. This probably ought to be for internal use only.
polyCache :: Map (Integer, Integer) (Poly Rating)
polyCache = M.fromList [((n1, n2), threeOutOfFive (beta n1 n2)) | n1 <- [0,4..84], n2 <- [0,4..84]]

-- TODO: taking out extra factors of (n1+n2-1)!/((n1-1)!(n2-1)!) could keep the
-- numbers a bit smaller
-- Type used to be this, before we wanted to cache polynomials:
-- drMarioProb :: (Eq a, Fractional a) => Integer -> Integer -> a -> a -> a
-- | The (modeled) probability of one player beating another in a match of Dr.
-- Mario, given the number of viruses each starts with and the rates that they
-- each clear viruses at.
drMarioProb :: Integer -> Integer -> Rating -> Rating -> Rating
drMarioProb n1 n2 r1 r2 = evalPoly poly (r1 / (r1+r2)) where
	poly = fromMaybe (threeOutOfFive (beta n1 n2)) (M.lookup (n1, n2) polyCache)

data E64
instance HasResolution E64 where resolution _ = 10^64

-- | The speed pills drop.
data Speed = Low | Medium | High
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A player's name.
type Name = Text

-- | A summary of how a match was set up for one of the two players involved.
data PlayerSettings = PlayerSettings
	{ name :: Name
	, level :: Integer
	, speed :: Speed
	}
	deriving (Eq, Ord, Read, Show)

-- | The settings used to play a match, along with who won.
data GameRecord = GameRecord
	{ winner, loser :: PlayerSettings
	, date :: UTCTime
	} deriving (Eq, Ord, Read, Show)

-- | A single player's rate of clearing viruses.
type Rating = Fixed E64

-- | The relative speeds of some players.
data Ratings = Ratings
	{ defaulted :: Name -- ^ An arbitrary distinguished player whose rating is fixed to 1.
	, learned :: Map Name Rating -- ^ Ratings of other players. Does not contain the value of 'defaulted' as a key.
	} deriving (Eq, Ord, Read, Show)

-- | We do gradient ascent on the likelihood of a given value for 'Ratings'.
-- This type is used internally to distinguish between the actual ratings and
-- the gradient of the ratings. (Also, the 'defaulted' field's meaning is
-- slightly modified: the gradient for this player is fixed to 0.)
type RatingsGradient = Ratings

-- | Get a given player's rating, if they have one.
getRating :: Ratings -> Name -> Maybe Rating
getRating rs n | n == defaulted rs = Just 1
               | otherwise         = M.lookup n (learned rs)

unsafeGetRating :: Ratings -> Name -> Rating
unsafeGetRating rs n = fromMaybe 1 (getRating rs n)

-- | Set a player's rating. It is assumed that the given name is not the
-- defaulted player. Ratings are clipped to be positive.
setRating :: Name -> Rating -> Ratings -> Ratings
setRating n r_ rs = rs { learned = M.insert n r (learned rs) } where
	-- using MkFixed 1 instead of 1e-64 makes sure we have the smallest
	-- possible positive value even if we change the resolution later
	r = max (MkFixed 1) r_

-- | Ideally, we'd like to maximize the likelihood of the game records we've
-- seen. But the usual problem with that is the likelihood drops quickly as you
-- see more games, and can easily underflow to 0 (especially when the ratings
-- are not yet close to correct). The usual fix is to consider log likelihood
-- instead, and add log-probabilities instead of multiplying probabilities.
-- We'll do something pretty similar, with a slight modification: before taking
-- the log, we'll multiply each probability by the available precision. As with
-- taking the log, this is a monotonic change, so optimizing this function is
-- the same as optimizing the log likelihood. We make this change so that we
-- don't arbitrarily drop all the precision we have when converting to
-- 'Double'; we convert to 'Double' because 'Rating' doesn't support 'log'.
--
-- It is assumed that if you construct the graph with players as nodes and game
-- records as undirected edges, you get a single connected component.
--
-- It is assumed that there is a rating for every player in the game records.
drMarioObjective :: [GameRecord] -> Ratings -> Double
drMarioObjective gs rs = sum $ map pointObjective gs where
	pointObjective g = log (fromInteger v) where
		r1 = unsafeGetRating rs (name (winner g))
		r2 = unsafeGetRating rs (name (loser  g))
		n1 = 4 * (level (winner g) + 1)
		n2 = 4 * (level (loser  g) + 1)
		MkFixed v = drMarioProb n1 n2 r1 r2

-- This function is conceptually simple: compute (f hi - f lo) / (hi - lo) for
-- each player, where f is a suitable call to drMarioObjective, hi is a
-- slightly higher rating for that player, and lo is a slightly lower rating
-- for that player. But the Maybe's and Double/E64 mismatches get in the way.
-- Try to see past the noise.

-- | Find the approximate gradient of the objective function numerically.
--
-- The standard assumptions apply (see 'drMarioObjective').
numericGradient :: [GameRecord] -> Ratings -> RatingsGradient
numericGradient gs rs = rs { learned = M.mapWithKey numGradAt (learned rs) } where
	numGradAt n r = realToFrac (f hi - f lo) / (hi - lo) where
		f x = drMarioObjective gs (setRating n x rs)
		hi = r * 1.00001
		lo = r / 1.00001

-- | Given a learning parameter, do one step of gradient ascent. Smaller
-- learning parameters learn more slowly, but at lower risk of oscillating
-- wildly.
--
-- The learning parameter is assumed to be positive.
--
-- The standard assumptions also apply (see 'drMarioObjective').
gradAscendStep :: Fixed E64 -> [GameRecord] -> Ratings -> Ratings
gradAscendStep rate gs rs = rs
	{ learned = M.intersectionWith
		(\v dv -> max (MkFixed 1) (v + dv*rate))
		(learned rs)
		(learned (numericGradient gs rs))
	}

-- | Given a learning parameter and a ratio, iterate gradient ascent until all
-- ratings have stabilized (are not changing by more than the ratio in either
-- direction).
--
-- The learning parameter is assumed to be positive, and the ratio is assumed
-- to be greater than 1.
--
-- The standard assumptions also apply (see 'drMarioObjective').
gradAscend :: Fixed E64 -> Fixed E64 -> [GameRecord] -> Ratings -> Ratings
gradAscend rate ratio gs rs
	| done = rs'
	| otherwise = gradAscend rate ratio gs rs'
	where
	rs' = gradAscendStep rate gs rs
	done = all (\(old, new) -> max old new / min old new < ratio)
	           (M.intersectionWith (,) (learned rs) (learned rs'))

-- TODO: Newton's method might be a lot better... fewer hand-tweaked
-- parameters, at least. Worth trying.

fast = T.pack "fast"
slow = T.pack "slow"
player1 = PlayerSettings fast 16 High
player2 = PlayerSettings slow  8 Medium
records = [ GameRecord player1 player2 undefined
          , GameRecord player2 player1 undefined
          ]
initialRatings = Ratings slow (M.singleton fast 1)

-- figure out how much faster a hypothetical fast player is than a hypothetical
-- slow player
main :: IO ()
main = print (gradAscend 1e-3 1.000001 records initialRatings)
