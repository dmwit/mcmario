module MCMario.Model
	( Rate
	, winProbability
	, tieProbability
	, fullPrecisionRate
	, epsilon
	, geometricMean
	, (%/)
	, (%*)
	) where

import Data.Fixed
import Data.List.Split
import Data.Ratio
import Math.NumberTheory.Powers.General

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

-- | A version of 'div' that rounds instead of flooring.
divRound :: Integral a => a -> a -> a
divRound a b = div (a + div b 2) b

infixl 7 %/, %*
-- | The implementation of '(/)' in base systematically underestimates its
-- result -- it does something akin to floor instead of round after computing
-- the division. This function does slightly better.
(%/) :: HasResolution a => Fixed a -> Fixed a -> Fixed a
fa@(MkFixed a) %/ MkFixed b = MkFixed (divRound (a * resolution fa) b)

-- | The implementation of '(*)' in base systematically underestimates its
-- result -- it does something akin to floor instead of round after computing
-- the multiplication. This function does slightly better.
(%*) :: HasResolution a => Fixed a -> Fixed a -> Fixed a
fa@(MkFixed a) %* MkFixed b = MkFixed (divRound (a * b) (resolution fa))

-- | Given a rate at which goals are scored, compute the unnormalized weights
-- of each possible number of goals. (Unnormalized: this computes the Poisson
-- distribution, but without the exponential factor that makes it sum to 1.)
goalCountWeights :: Rate -> [Rate]
goalCountWeights r = takeWhile (>0) $ scanl (\w i -> w%*r%/i) 1 [1,2..]

-- | Like 'exp', but actually works.
expRate :: Rate -> Rate
expRate = sum . goalCountWeights

-- | Given a handicap -- a factor by which player 2's score is multiplied --
-- and the rates at which player 1 and player 2 score goals, compute the
-- probability that player 1 wins.
winProbability :: Rational -> Rate -> Rate -> Rate
winProbability handicap r1 r2 = totalWeight %/ normalizationFactor where
	normalizationFactor = expRate (r1+r2)
	winningScoresForPlayer1 = map (floor . (1+)) . iterate (handicap+) $ 0
	winningScoreDeltasForPlayer1 = zipWith (-) (tail winningScoresForPlayer1) winningScoresForPlayer1
	weights1 = id
		. takeWhile (not . null)
		. scanl (flip drop) (drop 1 $ goalCountWeights r1)
		$ winningScoreDeltasForPlayer1
	weights2 = goalCountWeights r2
	totalWeight = sum . concat $ zipWith (\w1s w2 -> map (w2%*) w1s) weights1 weights2

-- | Given a handicap -- a factor by which player 2's score is multiplied --
-- and the rates at which player 1 and player 2 score goals, compute the
-- probability that they tie.
tieProbability :: Rational -> Rate -> Rate -> Rate
tieProbability handicap r1 r2 = totalWeight %/ normalizationFactor where
	normalizationFactor = expRate (r1+r2)
	spacedWeights n r = map head . chunksOf (fromInteger n) $ goalCountWeights r
	weights1 = spacedWeights (numerator   handicap) r1
	weights2 = spacedWeights (denominator handicap) r2
	totalWeight = sum $ zipWith (%*) weights1 weights2
