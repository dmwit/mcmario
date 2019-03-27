module MCMario.Model
	( Rate
	, gameProbability
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
import MCMario.GameDB

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

-- | Given a handicap -- a factor by which orange's score is multiplied -- and
-- the rates at which blue and orange score goals, compute the probability that
-- blue wins.
winProbability :: Rational -> Rate -> Rate -> Rate
winProbability handicap rBlue rOrange = totalWeight %/ normalizationFactor where
	normalizationFactor = expRate (rBlue+rOrange)
	winningScoresForBlue = map (floor . (1+)) . iterate (handicap+) $ 0
	winningScoreDeltasForBlue = zipWith (-) (tail winningScoresForBlue) winningScoresForBlue
	weightsBlue = id
		. takeWhile (not . null)
		. scanl (flip drop) (drop 1 $ goalCountWeights rBlue)
		$ winningScoreDeltasForBlue
	weightsOrange = goalCountWeights rOrange
	totalWeight = sum . concat $ zipWith (\wsBlue wOrange -> map (wOrange%*) wsBlue) weightsBlue weightsOrange

-- | Given a handicap -- a factor by which orange's score is multiplied -- and
-- the rates at which blue and orange score goals, compute the probability that
-- they tie.
tieProbability :: Rational -> Rate -> Rate -> Rate
tieProbability handicap rBlue rOrange = totalWeight %/ normalizationFactor where
	normalizationFactor = expRate (rBlue+rOrange)
	spacedWeights n r = map head . chunksOf (fromInteger n) $ goalCountWeights r
	weightsBlue   = spacedWeights (numerator   handicap) rBlue
	weightsOrange = spacedWeights (denominator handicap) rOrange
	totalWeight = sum $ zipWith (%*) weightsBlue weightsOrange

-- TODO: This has an odd behavior, I think. Namely: it penalizes players who
-- choose to participate in team games for tying the game. Since we add the
-- rates of the players on the teams, this drastically reduces the probability
-- of a tie; so this model probably underestimates the probability of
-- well-composed teams tying. (Even teams of absolutely top-notch players tie,
-- just think of how many of the videos you've watched on YouTube of pro
-- players end up going into overtime.)
--
-- Perhaps we can fix this by shifting the interpretation of the rate from
-- being a rate of goals to a rate of shots, and adding a parameter to each
-- player for the probability that any given shot makes it into net. This way,
-- teams would have higher shot rates, yes, but they'd also have lower chances
-- of letting shots into goal, and ties would become likely again. To avoid
-- being able to inflate these indefinitely without changing predictions, we
-- can clip the shot success rate parameter to being between 0 and 1 (well,
-- epsilon and 1-epsilon, probably).
--
-- How to compose parameters on a team? Shot rates add as before. For shot
-- success rates, I think it makes sense to allow a shot to score if everybody
-- on the other team gets a chance to block and fails. So the success rates
-- multiply.

-- | Given the handicap -- a factor by which orange's score is multiplied --
-- and the rates at which the blue team and orange team score goals, compute
-- the probability of the given game outcome.
gameProbability :: Winner -> Rational -> Rate -> Rate -> Rate
gameProbability w handicap rBlue rOrange = case w of
	Blue   -> winProbability handicap rBlue rOrange
	Tie    -> tieProbability handicap rBlue rOrange
	Orange -> winProbability (recip handicap) rOrange rBlue
