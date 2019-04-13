module MCMario.RatingDB
	( RatingDB
	, Rate
	, Confidence(..)
	, Matchup(..)
	, improveComponentRatings
	, matchup
	, unsafeMatchup
	) where

import Control.DeepSeq
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Ratio
import Data.Set (Set)
import MCMario.GameDB
import MCMario.Model
import qualified Data.Map as M
import qualified Data.Set as S

-- | Information used for choosing a handicap, organized by player. This type
-- is intended to be abstract; don't rely on it being a @Map Name Rating@.
type RatingDB  = Map Name Rate
type Gradients = Map Name Rate

-- | The rating we use by default if (perhaps by some bug) they don't have a
-- rating when we go to look for one.
defaultRating :: Rate
defaultRating = 3

-- | The highest score multiplier we're willing to consider.
maxMultiplier :: Integer
maxMultiplier = 10

-- TODO: mini-batching

-- | Improve the ratings for a single connected component of the game graph.
improveComponentRatings :: GameDB -> RatingDB -> Component -> [RatingDB]
improveComponentRatings gdb rdb ns = id
	. map (M.unionWith (\old new -> new) rdb)
	. gradAscend 1e-2 1.5 (componentGames gdb ns)
	. M.union (M.restrictKeys rdb ns)
	. M.fromSet (const defaultRating)
	$ ns

-- | Set somebody's rate. Does some minor sanity checking to make sure the rate
-- is positive.
setRate :: Name -> Rate -> RatingDB -> RatingDB
setRate n = M.insert n . max epsilon

-- | We use the log-likelihood of the game records we've seen as the objective.
-- Any player with no 'Rate' is given some sensible default.
objective :: [GameRecord] -> RatingDB -> Double
objective gs rs = sum (map gameObjective gs) where
	gameObjective g = log (clipProb p) where
		rBlue   = teamRating (blue   g)
		rOrange = teamRating (orange g)
		p = gameProbability (winner g) (handicap g) rBlue rOrange

	-- log 0 is -Infinity, that's right out
	-- log 1 is perfectly fine, but let's avoid it anyway for symmetry
	clipProb = min 0.9999999999999999 . max epsilon
	playerRating n = M.findWithDefault defaultRating n rs
	teamRating = sum . map playerRating . S.toList

-- TODO: Currently, when finding the gradient at player X, we compute the
-- objective function over all games in X's component. But actually the
-- gradient for X only depends on the games X participated in, so we could
-- compute the objective on a much smaller set of games without affecting the
-- gradient. This should be more efficient by a pretty fair margin. For bonus
-- points, only partition the game database once for each player and not once
-- for each player and each iteration of gradient ascent.

-- | Find the approximate gradient of the objective function numerically.
numericGradient :: [GameRecord] -> RatingDB -> Gradients
numericGradient gs rs = M.mapWithKey numGradAt rs where
	numGradAt n r = (f hi - f lo) / (hi - lo) where
		f x = objective gs (setRate n x rs)
		hi = r * 1.00001
		lo = r / 1.00001

-- TODO: This adjustmentFactor stuff is assuming everything is positive; should
-- be bother being worried about that? Currently:
-- 1. The adjustmentFactor is a constant, chosen by the caller, and our only
--    caller chooses a positive constant.
-- 2. The 'defaultRating' is positive.
-- 3. Having a rating switch sign is not sane.
-- So I think it's okay for now. But if any of the above assumptions get
-- invalidated in the future we might be in trouble!

-- | Given a learning parameter and a maximum adjustment factor, do one step of
-- gradient ascent. Smaller learning parameters learn more slowly, but at lower
-- risk of oscillating wildly. The maximum adjustment factor can also be used
-- to avoid oscillations: each individual rate will be clipped to be within
-- this factor of its previous value.
--
-- The learning parameter and adjustment factor are assumed to be positive.
gradAscendStep :: Rate -> Rate -> [GameRecord] -> RatingDB -> RatingDB
gradAscendStep learningRate adjustmentFactor gs rs = M.intersectionWith
	(\v dv -> clip v (v + dv*learningRate))
	rs
	(numericGradient gs rs)
	where
	clip v = id
		. max epsilon
		. max (v/adjustmentFactor)
		. min (v*adjustmentFactor)

-- TODO: look into using SBV's SReal for the optimization problem here

-- | Given a learning parameter and an adjustment factor, iterate gradient
-- ascent. As with a single step of gradient ascent, smaller learning
-- parameters learn more slowly, but at lower risk of oscillating wildly. The
-- maximum adjustment factor can also be used to avoid oscillations: at each
-- step of gradient ascent, each individual rate will be clipped to be within
-- this factor of its previous value.
--
-- The learning parameter and adjustment factor are assumed to be positive.
gradAscend :: Rate -> Rate -> [GameRecord] -> RatingDB -> [RatingDB]
gradAscend learningRate adjustmentFactor gs rs
	= iterate (gradAscendStep learningRate adjustmentFactor gs) rs

-- TODO: Newton's method might be a lot better... fewer hand-tweaked
-- parameters, at least. Worth trying.

-- | We will try to suggest a matchup for any pair of players. But if the
-- players are not in the same 'Component', then we can't really be sure about
-- what settings would be good, and we just use a heuristic to make a guess.
-- This type is for reporting whether we used a heuristic or a principled way
-- of arriving at a given matchup.
data Confidence = Provisional | Confident deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A recommendation about what settings two players should choose, along with
-- backup plans for level settings if they want longer/shorter games than the
-- most fair possible settings would give them.
data Matchup = Matchup
	{ left, right :: Integer
	, leftToRight, rightToLeft :: Map Integer Integer
	, confidence :: Confidence
	} deriving (Eq, Ord, Read, Show)

-- | Suggest player settings, using some combination of precomputed information
-- in the 'RatingDB' given and manual queries to the 'GameDB' given.
matchup :: GameDB -> RatingDB -> Set Name -> Set Name -> Matchup
matchup games ratings lNames rNames = case S.lookupMin lNames >>= flip M.lookup componentMap of
	Just component | allNames `S.isSubsetOf` component
	  -> confident   $ unsafeMatchup lRating  rRating
	_ -> provisional $ unsafeMatchup lRating' rRating'
	where
	allNames = S.union lNames rNames
	componentMap = M.unions [M.fromSet (const c) c | c <- components games]
	lRating = totalRating lNames
	rRating = totalRating rNames
	confident   m = m { confidence = Confident   }
	provisional m = m { confidence = Provisional }
	totalRating = id
		. sum
		. map (\name -> M.findWithDefault defaultRating name ratings)
		. S.toList

	-- Heuristic: normalize rates by dividing by each player's component's (geometric)
	-- average rating. Then count how many games involve players on one team
	-- beating players on the other team, boosting the appropriate team's
	-- rating by 30% for each. (This way if players play each other while not
	-- being in the same melee, winning repeatedly will still change the
	-- handicap.)
	lWinEdges = edgesBetween games lNames rNames
	rWinEdges = edgesBetween games rNames lNames
	meanRating name = case M.lookup name componentMap of
		Just component -> geometricMean (ratings `M.restrictKeys` component)
		Nothing -> defaultRating
	normalizedTotalRating names = sum
		[ M.findWithDefault defaultRating name ratings / meanRating name
		| name <- S.toList names
		]
	lNormalizedRating = normalizedTotalRating lNames
	rNormalizedRating = normalizedTotalRating rNames
	lRating' = normalizedTotalRating lNames * 1.3^length lWinEdges
	rRating' = normalizedTotalRating rNames * 1.3^length rWinEdges

-- | Internal use only. Given some ratings, produce a matchup. It's on you to
-- make sure the ratings can be sensibly compared (i.e. are from the same
-- 'Component'). Unsafe because it doesn't set the 'confidence' of the output.
unsafeMatchup :: Rate -> Rate -> Matchup
unsafeMatchup lRating rRating = Matchup
	{ left  = numerator   ratio
	, right = denominator ratio
	, leftToRight = M.fromAscList $ map (fmap fst) lToR
	, rightToLeft = M.fromAscList $ map (fmap fst) rToL
	, confidence = error "drat, somebody forgot to fix up the confidence field after calling unsafeMatchup"
	}
	where
	ratio = lMultiplier % rMultiplier
	(lMultiplier, (rMultiplier, _)) = maximumBy (comparing (normalizeQuality . snd . snd)) lToR
	lToR = [(multiplier, bestMatch lRating rRating multiplier) | multiplier <- [1..maxMultiplier]]
	rToL = [(multiplier, bestMatch rRating lRating multiplier) | multiplier <- [1..maxMultiplier]]

-- | If @bestMatch r1 r2 multiplier1 = (multiplier2, quality)@, then
--
-- * 1 <= @multiplier2@ <= @maxMultiplier@
-- * the win-loss ratio for a player making goals worth @multiplier1@ points
--   each at rate @r1@ vs a player making goals worth @multiplier2@ points each
--   at rate @r2@ is as close to 1 as possible
-- * @quality@ is the win-loss ratio for player 1
bestMatch :: Rate -> Rate -> Integer -> (Integer, Rate)
bestMatch r1 r2 multiplier1 = case quality r1 r2 (guess % multiplier1) of
	p | p > 1     -> searchUp   guess p
	  | otherwise -> searchDown guess p
	where
	clip = max 1 . min maxMultiplier
	guess = clip . floor $ r1 * fromInteger multiplier1 / r2

	searchUp guess p | guess == maxMultiplier = (guess, p)
	searchUp guess p = case quality r1 r2 ((guess+1) % multiplier1) of
		p' | p' > 1    -> searchUp (guess+1) p'
		   | otherwise -> if normalizeQuality p > normalizeQuality p'
		   	then (guess, p)
		   	else (guess+1, p')

	searchDown 1 p = (1, p)
	searchDown guess p = case quality r1 r2 ((guess-1) % multiplier1) of
		p' | p' <= 1  -> searchDown (guess-1) p'
		   | otherwise -> if normalizeQuality p > normalizeQuality p'
		   	then (guess, p)
		   	else (guess-1, p')

-- | Same behavior as 'bestMatch', but implemented in an obviously correct way.
-- Good for testing correctness of 'bestMatch'.
bestMatchSlow :: Rate -> Rate -> Integer -> (Integer, Rate)
bestMatchSlow r1 r2 multiplier1 = id
	. maximumBy (comparing (normalizeQuality.snd))
	. map (\multiplier2 -> (multiplier2, quality r1 r2 (multiplier2 % multiplier1)))
	$ [1..maxMultiplier]

normalizeQuality :: Rate -> Rate
normalizeQuality p | p > 1 = 1/p | otherwise = p

quality :: Rate -> Rate -> Rational -> Rate
quality r1 r2 handicap = win / lose where
	-- The win and loss probabilities cost about the same to compute, but the
	-- tie probability is much cheaper. So if we have to compute two
	-- probabilities, we want one of them to be the tie probability.
	win = gameProbability Blue handicap r1 r2
	tie = gameProbability Tie  handicap r1 r2
	lose = 1-win-tie
