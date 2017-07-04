module Main where

import Data.List
import Data.Ord
import Math.Polynomial
import System.Environment

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

-- TODO: taking out extra factors of (n1+n2-1)!/((n1-1)!(n2-1)!) could keep the
-- numbers a bit smaller
-- | The (modeled) probability of one player beating another in a match of Dr.
-- Mario, given the number of viruses each starts with and the rates that they
-- each clear viruses at.
drMarioProb :: (Eq a, Fractional a) => Integer -> Integer -> a -> a -> a
drMarioProb n1 n2 r1 r2 = evalPoly (threeOutOfFive (beta n1 n2)) (r1 / (r1+r2))

-- show good matchups between players where one player is thrice as fast as the other
main :: IO ()
main = do
	[r] <- map (toRational . (read :: String -> Double)) <$> getArgs
	mapM_ print . sortBy (comparing (\(_,_,p) -> abs (p-0.5))) $ do
		lo <- [4,8..84]
		hi <- [4,8..84]
		return (lo`div`4 - 1, hi`div`4 - 1, fromRational (drMarioProb lo hi r 1))
