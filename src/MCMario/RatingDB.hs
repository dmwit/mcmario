module MCMario.RatingDB
	( RatingDB
	, Rating
	, Confidence(..)
	, Matchup(..)
	, inferRatings
	, matchup
	) where

import Control.DeepSeq
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import MCMario.GameDB
import MCMario.Model
import qualified Data.Map as M
import qualified Data.Set as S

-- | Information used for choosing a handicap.
data Rating = Rating
	{ rate      :: Rate      -- ^ how quickly this person clears viruses compared to others in their melee
	, melee     :: Component -- ^ a group of people who each have winning and losing paths to each other, in some canonical order; 'rate's are comparable within a 'melee', but not across 'melee's
	, speedPref :: Speed     -- ^ a hack: precomputed most-frequent speed used by this person, stuffed in here so we don't have to figure it out afresh on each game
	} deriving (Eq, Ord, Read, Show)

instance NFData Rating where
	rnf (Rating r m s) = rnf r `seq` rnf m `seq` rnf s

-- | Information used for choosing a handicap, organized by player. This type
-- is intended to be abstract; don't rely on it being a @Map Name Rating@.
type RatingDB  = Map Name Rating
type Rates     = Map Name Rate
type Gradients = Map Name Rate

-- | Use gradient ascent to infer ratings for each player from their win/loss
-- record against other players.
inferRatings :: GameDB -> RatingDB
inferRatings db = id
	. M.unions
	. map (inferComponentRatings db)
	. components
	$ db

-- | Infer the ratings for a single connected component of the game graph.
inferComponentRatings :: GameDB -> Component -> RatingDB
inferComponentRatings db ns = id
	. M.mapWithKey (\n r -> Rating r ns (preferredSpeed db n))
	. M.insert chosenName 1
	. gradAscend 1e-3 1.000001 (componentGames db ns)
	. M.fromSet (const 1)
	$ otherNames
	where
	(chosenName, otherNames) = S.deleteFindMin ns

preferredSpeed :: GameDB -> Name -> Speed
preferredSpeed db n = id
	. fst
	. head
	. sortBy (comparing (negate . snd))
	. ((Medium,0):) -- so that there is always at least one entry
	. M.toList
	. M.fromListWith (+)
	$ [ (speed settings, 1)
	  | r <- playerGames db n
	  , settings <- [winner r, loser r]
	  , name settings == n
	  ]

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
objective gs rs = sum $ map gameObjective gs where
	gameObjective g = log (fromInteger v) where
		l1 = level (winner g)
		l2 = level (loser  g)
		r1 = M.findWithDefault 1 (name (winner g)) rs
		r2 = M.findWithDefault 1 (name (loser  g)) rs
		v  = fullPrecisionRate (matchProbByVirusLevel l1 l2 r1 r2)

-- | Find the approximate gradient of the objective function numerically.
numericGradient :: [GameRecord] -> Rates -> Gradients
numericGradient gs rs = M.mapWithKey numGradAt rs where
	numGradAt n r = realToFrac (f hi - f lo) / (hi - lo) where
		f x = objective gs (setRate n x rs)
		hi = r * 1.00001
		lo = r / 1.00001

-- TODO: perhaps we should divide by the number of games each player has
-- played (or something like that), to counteract the effect that playing more
-- games increases your sensitivity to rating changes, giving you a higher
-- gradient

-- | Given a learning parameter, do one step of gradient ascent. Smaller
-- learning parameters learn more slowly, but at lower risk of oscillating
-- wildly.
--
-- The learning parameter is assumed to be positive.
gradAscendStep :: Rate -> [GameRecord] -> Rates -> Rates
gradAscendStep learningRate gs rs = M.intersectionWith
	(\v dv -> max epsilon (v + dv*learningRate))
	rs
	(numericGradient gs rs)

-- | Given a learning parameter and a ratio, iterate gradient ascent until all
-- ratings have stabilized (are not changing by more than the ratio in either
-- direction).
--
-- The learning parameter is assumed to be positive, and the ratio is assumed
-- to be greater than 1.
gradAscend :: Rate -> Rate -> [GameRecord] -> Rates -> Rates
gradAscend learningRate ratio gs rs
	| done = rs'
	| otherwise = gradAscend learningRate ratio gs rs'
	where
	rs' = gradAscendStep learningRate gs rs
	done = all (\(old, new) -> max old new / min old new < ratio)
	           (M.intersectionWith (,) rs rs')

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
-- in the 'RatingDB' given and manual queries to the 'GameDB' given.
matchup :: GameDB -> RatingDB -> Name -> Name -> Matchup
matchup games ratings lName rName
	| lMelee == rMelee = confident   $ unsafeMatchup lName lRating  rName rRating
	| otherwise        = provisional $ unsafeMatchup lName lRating' rName rRating'
	where
	defRating n = Rating 1 (S.singleton n) Medium
	lRating = M.findWithDefault (defRating lName) lName ratings
	rRating = M.findWithDefault (defRating rName) rName ratings
	lMelee = melee lRating
	rMelee = melee rRating
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
		. fmap rate
		. M.insert name rating -- in case M.findWithDefault had to use the default
		$ ratings `M.restrictKeys` melee
	lMeanRate = meanRate lName lRating lMelee
	rMeanRate = meanRate rName rRating rMelee
	lRating' = lRating { rate = rate lRating / lMeanRate * 1.3^length lWinEdges }
	rRating' = rRating { rate = rate rRating / rMeanRate * 1.3^length rWinEdges }

-- | Internal use only. Given some names and ratings, produce a matchup. Unsafe for two reasons:
--
-- 1. Completely ignores the 'melee' of the two ratings. If the 'rate's aren't
-- comparable, you'll get garbage outputs.
-- 2. Doesn't set the 'confidence' of the output.
unsafeMatchup :: Name -> Rating -> Name -> Rating -> Matchup
unsafeMatchup lName lRating rName rRating = Matchup
	{ left = PlayerSettings
		{ name  = lName
		, level = lLevel
		, speed = speedPref lRating
		}
	, right = PlayerSettings
		{ name  = rName
		, level = rLevel
		, speed = speedPref rRating
		}
	, leftToRight = M.fromAscList $ map (fmap fst) lToR
	, rightToLeft = M.fromAscList $ map (fmap fst) rToL
	, confidence = error "drat, somebody forgot to fix up the confidence field after calling unsafeMatchup"
	}
	where
	(lLevel, (rLevel, _)) = minimumBy (comparing (abs . (0.5-) . snd . snd)) lToR
	lToR = [(level, bestMatch (rate lRating) (rate rRating) level) | level <- [0..20]]
	rToL = [(level, bestMatch (rate rRating) (rate lRating) level) | level <- [0..20]]

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
