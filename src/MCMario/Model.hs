module MCMario.Model
	( Rate
	, fullPrecisionRate
	, epsilon
	, geometricMean
	, matchProbByVirusCount
	, matchProbByVirusLevel
	,  gameProbByVirusCount
	,  gameProbByVirusLevel
	) where

import Data.Fixed
import Data.List
import Data.Map (Map)
import Data.Maybe
import Math.NumberTheory.Powers.General
import Math.Polynomial
import qualified Data.Map as M

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

-- | For numbers stored as multiples of 1e-100.
data E100
instance HasResolution E100 where resolution _ = 10^100

-- | A measure of how quickly a player clears viruses. (The parameter to a
-- Poisson or exponential distribution.) Functions that accept one of these
-- expect it to be positive.
--
-- You are intended to treat this type as abstract (i.e. not know that it's
-- actually a @Fixed E100@).
type Rate = Fixed E100

-- | @(/epsilon)@, but efficiently.
fullPrecisionRate :: Rate -> Integer
fullPrecisionRate (MkFixed v) = v

-- | The smallest positive rate.
epsilon :: Rate
epsilon = MkFixed 1

-- TODO: address https://stackoverflow.com/q/45067989/791604
-- (if it hasn't been fixed upstream already)
-- | Take the geometric mean with all the available precision. Assumes a
-- non-empty container.
geometricMean :: (Functor f, Foldable f) => f Rate -> Rate
geometricMean rs
	| null rs = error "geometricMean called on empty container"
	| otherwise = id
	. MkFixed
	. integerRoot (length rs)
	. product
	. fmap (\(MkFixed i) -> i) -- TODO: coerce?
	$ rs

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

-- TODO: taking out extra factors of (n1+n2-1)!/((n1-1)!(n2-1)!) could keep the
-- numbers a bit smaller
-- Type used to be this, before we wanted to cache polynomials:
-- matchProbByVirusCount :: (Eq a, Fractional a) => Integer -> Integer -> a -> a -> a
-- | The (modeled) probability of one player beating another in a match of Dr.
-- Mario, given the number of viruses each starts with and the rates that they
-- each clear viruses at.
--
-- Things not currently modeled:
--
-- * Getting better (or worse) over time.
-- * How good you are at getting combos and sending junk.
-- * Losing (i.e. topping out).
-- * Higher levels are harder not just because they have more viruses, but also
--   because the viruses can appear higher in the bottle, reducing the time you
--   have to make a decision about where to put your piece -- and, in extreme
--   cases, sometimes making it difficult to even get a piece to a given
--   position.
-- * Related to the "how fast you can make decisions" thing: the speed
--   (HI/MED/LOW) isn't accounted for at all.
matchProbByVirusCount :: Integer -> Integer -> Rate -> Rate -> Rate
matchProbByVirusCount n1 n2 r1 r2 = evalPoly poly (r1 / (r1+r2)) where
	poly = fromMaybe (threeOutOfFive (beta n1 n2)) (M.lookup (n1, n2) matchPolyCache)

-- | Like 'matchProbByVirusCount', except it takes Dr. Mario virus levels from
-- 0-20 rather than virus counts.
matchProbByVirusLevel :: Integer -> Integer -> Rate -> Rate -> Rate
matchProbByVirusLevel level1 level2 = matchProbByVirusCount (4*level1 + 4) (4*level2 + 4)

-- | The (modeled) probability of one player beating another in a single round
-- of Dr. Mario (which would serve as part of a best-of-five in a usual match),
-- given the number of viruses each starts with and the rates that they each
-- clear viruses at.
gameProbByVirusCount :: Integer -> Integer -> Rate -> Rate -> Rate
gameProbByVirusCount n1 n2 r1 r2 = evalPoly poly (r1 / (r1+r2)) where
	poly = fromMaybe (beta n1 n2) (M.lookup (n1, n2) gamePolyCache)

-- | Like 'gameProbByVirusCount', except it takes Dr. Mario virus levels from
-- 0-20 rather than virus counts.
gameProbByVirusLevel :: Integer -> Integer -> Rate -> Rate -> Rate
gameProbByVirusLevel level1 level2 = gameProbByVirusCount (4*level1 + 4) (4*level2 + 4)

-- | There are only a couple hundred polynomials we ever actually use. No sense
-- recomputing them a lot. This probably ought to be for internal use only.
matchPolyCache :: Map (Integer, Integer) (Poly Rate)
matchPolyCache = threeOutOfFive <$> gamePolyCache

-- | See 'matchPolyCache'.
gamePolyCache :: Map (Integer, Integer) (Poly Rate)
gamePolyCache = M.fromList [((n1, n2), beta n1 n2) | n1 <- [4,8..84], n2 <- [4,8..84]]
