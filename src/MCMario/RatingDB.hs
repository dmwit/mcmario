module MCMario.RatingDB
	( RatingDB
	, Confidence(..)
	, Matchup(..)
	, improveRatings
	, matchup
	) where

import Control.DeepSeq
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import MCMario.GameDB.QueryCache
import MCMario.Model
import qualified Data.Map as M
import qualified Data.Set as S

-- | Information used for choosing a handicap, organized by player. This type
-- is intended to be abstract; don't rely on it being a @Map Name Rate@.
type RatingDB  = Map Name Rate
type Rates     = Map Name Rate -- partial DB: some keys may be missing
type Gradients = Map Name Rate -- contains derivatives of rates, not rates

-- | Use gradient ascent to improve the estimated ratings for each player from
-- their win/loss record against other players. Does a given number of steps of
-- gradient ascent.
improveRatings :: QueryCache -> RatingDB -> [RatingDB]
improveRatings gdb rdb = id
	. (++repeat rdb) -- funny edge case: if the GameDB is empty, there are no
	                 -- components and we get an empty list of RatingDBs
	                 -- instead of an infinite one
	. map M.unions
	. transpose
	. map (improveComponentRatings gdb rdb)
	. components
	$ gdb

-- | Improve the ratings for a single connected component of the game graph.
improveComponentRatings :: QueryCache -> RatingDB -> Component -> [RatingDB]
improveComponentRatings gdb rdb ns = id
	. map (M.insert chosenName 1)
	. gradAscend 1e-2 1.5 gdb
	. M.union (M.restrictKeys rdb otherNames)
	. M.fromSet (const 1)
	$ otherNames
	where
	(chosenName, otherNames) = S.deleteFindMin ns

-- | Set somebody's rate. Does some minor sanity checking to make sure the rate
-- is positive.
setRate :: Name -> Rate -> Rates -> Rates
setRate n = M.insert n . max epsilon

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
-- 'Double'; we convert to 'Double' because 'Rate' doesn't support 'log'.
--
-- Any player with no 'Rate' is given a 'Rate' of 1.
objective :: [GameRecord] -> Rates -> Double
objective gs rs = sum (map gameObjective gs) / genericLength gs where
	gameObjective g = log (fromInteger v) where
		l1 = level (winner g)
		l2 = level (loser  g)
		r1 = M.findWithDefault 1 (name (winner g)) rs
		r2 = M.findWithDefault 1 (name (loser  g)) rs
		v  = fullPrecisionRate . clipProb $ matchProbByVirusLevel l1 l2 r1 r2

	-- log 0 is -Infinity, that's right out
	-- log 1 is perfectly fine, but let's avoid it anyway for symmetry
	clipProb = min (1-epsilon) . max epsilon

-- TODO: Currently, when finding the gradient at player X, we compute the
-- objective function over all games in X's component. But actually the
-- gradient for X only depends on the games X participated in, so we could
-- compute the objective on a much smaller set of games without affecting the
-- gradient. This should be more efficient by a pretty fair margin. For bonus
-- points, only partition the game database once for each player and not once
-- for each player and each iteration of gradient ascent.

-- | Find the approximate gradient of the objective function numerically.
numericGradient :: QueryCache -> Rates -> Gradients
numericGradient gs rs = M.mapWithKey numGradAt rs where
	numGradAt n r = realToFrac (f hi - f lo) / (hi - lo) where
		f x = objective (playerComponentGames gs n) (setRate n x rs)
		hi = r * 1.00001
		lo = r / 1.00001

-- TODO: This adjustmentFactor stuff is assuming everything is positive; should
-- be bother being worried about that? Currently:
-- 1. The adjustmentFactor is a constant, chosen by the caller, and our only
--    caller chooses a positive constant.
-- 2. The default rating is 1, which is positive.
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
gradAscendStep :: Rate -> Rate -> QueryCache -> Rates -> Rates
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
gradAscend :: Rate -> Rate -> QueryCache -> Rates -> [Rates]
gradAscend learningRate adjustmentFactor gs rs
	= iterate (gradAscendStep learningRate adjustmentFactor gs) rs

-- TODO: Newton's method might be a lot better... fewer hand-tweaked
-- parameters, at least. Worth trying.

-- | We will try to suggest a matchup for any pair of players. But if the
-- players are not in the same 'melee', then we can't really be sure about what
-- settings would be good, and we just use a heuristic to make a guess. This
-- type is for reporting whether we used a heuristic or a principled way of
-- arriving at a given matchup.
data Confidence = Provisional | Confident deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A recommendation about what settings two players should choose, along with
-- backup plans for level settings if they want longer/shorter games than the
-- most fair possible settings would give them.
data Matchup = Matchup
	{ left, right :: PlayerSettings
	, leftToRight, rightToLeft :: Map Integer Integer
	, confidence :: Confidence
	} deriving (Eq, Ord, Read, Show)

-- | Suggest player settings, using some combination of precomputed information
-- in the 'RatingDB' given and manual queries to the 'QueryCache' given.
matchup :: QueryCache -> RatingDB -> Name -> Name -> Matchup
matchup games ratings lName rName
	| lMelee == rMelee = confident   $ unsafeMatchup games lName lRating  rName rRating
	| otherwise        = provisional $ unsafeMatchup games lName lRating' rName rRating'
	where
	lRating = M.findWithDefault 1 lName ratings
	rRating = M.findWithDefault 1 rName ratings
	lMelee = melee games lName
	rMelee = melee games rName
	confident   m = m { confidence = Confident   }
	provisional m = m { confidence = Provisional }

	-- Heuristic: normalize rates by dividing by the melee's (geometric)
	-- average rate. Then, if there are any games between players in the two
	-- melees, boost the winning melee's player's rating by 30% for each game.
	-- (This way if two players play each other while not being in the same
	-- melee, winning repeatedly will still change the handicap.)
	(lWinEdges, rWinEdges) = partition (\r -> name (winner r) `S.member` lMelee)
	                       $ edgesBetween games lMelee rMelee
	meanRate name rating melee = id
		. geometricMean
		. M.insert name rating -- in case the player doesn't exist in the DB yet
		$ ratings `M.restrictKeys` melee
	lMeanRate = meanRate lName lRating lMelee
	rMeanRate = meanRate rName rRating rMelee
	lRating' = lRating / lMeanRate * 1.3^length lWinEdges
	rRating' = rRating / rMeanRate * 1.3^length rWinEdges

-- | Internal use only. Given some names and ratings, produce a matchup. Unsafe for two reasons:
--
-- 1. Completely ignores the 'melee' of the two ratings. If the 'rate's aren't
-- comparable, you'll get garbage outputs.
-- 2. Doesn't set the 'confidence' of the output.
unsafeMatchup :: QueryCache -> Name -> Rate -> Name -> Rate -> Matchup
unsafeMatchup qc lName lRating rName rRating = Matchup
	{ left = PlayerSettings
		{ name  = lName
		, level = lLevel
		, speed = speedPref qc lName
		}
	, right = PlayerSettings
		{ name  = rName
		, level = rLevel
		, speed = speedPref qc rName
		}
	, leftToRight = M.fromAscList $ map (fmap fst) lToR
	, rightToLeft = M.fromAscList $ map (fmap fst) rToL
	, confidence = error "drat, somebody forgot to fix up the confidence field after calling unsafeMatchup"
	}
	where
	-- TODO: think about a less ad-hoc way of choosing (7,7); perhaps a simple
	-- cost model that penalizes "extreme" matchups like 0/0 or 20/20
	(lLevel, (rLevel, _))
		| lRating == rRating = (7, (7, error "wow, observing this seems especially impossible"))
		| otherwise = minimumBy (comparing (abs . (0.5-) . snd . snd)) lToR
	lToR = [(level, bestMatch lRating rRating level) | level <- [0..20]]
	rToL = [(level, bestMatch rRating lRating level) | level <- [0..20]]

-- | If @bestMatch r1 r2 level1 = (level2, prob)@, then
--
-- * 0 <= @level2@ <= 20
-- * the win ratio for a player clearing viruses at rate @r1@ on level @level1@
--   vs. a player clearing viruses at rate @r2@ on level @level2@ is as close to
--   50% as possible
-- * @prob@ is the probability of player 1 winning each round of the match
bestMatch :: Rate -> Rate -> Integer -> (Integer, Rate)
bestMatch r1 r2 level1 = case gameProbByVirusLevel level1 guess r1 r2 of
	p | p <= 0.5  -> searchUp   guess p
	  | otherwise -> searchDown guess p
	where
	clip = max 0 . min 20
	guess = clip (floor (r2 * (fromInteger (level1 + 1)) / r1) - 1)

	searchUp 20 p = (20, p)
	searchUp guess p = case gameProbByVirusLevel level1 (guess+1) r1 r2 of
		p' | p' <= 0.5 -> searchUp (guess+1) p'
		   | otherwise -> if p'-0.5 > 0.5-p then (guess, p) else (guess+1, p')

	searchDown 0 p = (0, p)
	searchDown guess p = case gameProbByVirusLevel level1 (guess-1) r1 r2 of
		p' | p' > 0.5 -> searchDown (guess-1) p'
		   | otherwise -> if 0.5-p' > p-0.5 then (guess, p) else (guess-1, p')

-- | Same behavior as 'bestMatch', but implemented in an obviously correct way.
-- Good for testing correctness of 'bestMatch'.
bestMatchSlow :: Rate -> Rate -> Integer -> (Integer, Rate)
bestMatchSlow r1 r2 level1 = id
	. minimumBy (\(_, p) (_, p') -> compare (abs (p-0.5)) (abs (p'-0.5)))
	. map (\level2 -> (level2, gameProbByVirusLevel level1 level2 r1 r2))
	$ [0..20]
