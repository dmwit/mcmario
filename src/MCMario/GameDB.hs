{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module MCMario.GameDB
	( Name
	, Winner(..)
	, GameRecord(..)
	, GameDB
	, Component
	, addGame
	, listPlayers
	, listGames
	, playerGames
	, components
	, componentGames
	, edgesBetween
	) where

import Control.DeepSeq
import Control.Monad.State
import Data.Default
import Data.Either
import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Graph (Graph, Node)
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as G
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S

-- | A player's name.
type Name = Text

data Winner = Blue | Tie | Orange deriving (Bounded, Enum, Eq, Ord, Read, Show)
instance NFData Winner where rnf x = x `seq` ()

-- | The settings used to play a match, along with who won.
--
-- The right team's score is multiplied by the handicap to determine the
-- winner.
data GameRecord = GameRecord
	{ blue, orange :: Set Name
	, handicap :: Rational
	, winner :: Winner
	, date :: UTCTime
	} deriving (Eq, Ord, Read, Show)

instance NFData GameRecord where
	rnf (GameRecord b o h w d) = rnf b `seq` rnf o `seq` rnf h `seq` rnf w `seq` rnf d

-- Invariants:
--
-- 1. The graph is bipartite, with Name nodes in one half and GameRecord nodes
--    in the other.
-- 2. Tie games have outgoing edges to each Name on both teams (and no other
--    outgoing edges) and no incoming edges.
-- 3. All other games have incoming edges from each Name on the winning team
--    and outgoing edges to each Name on the losing team (and no other incident
--    edges).
-- 4. Node i has label Left n in games iff key n has value i in nodes.

-- | A record of all games played.
data GameDB = GameDB
	{ games :: Gr (Either Name GameRecord) ()
	, nodes :: Map Name Node
	} deriving (Eq, Read, Show)

instance Default (Gr a b) where def = G.empty
instance Default GameDB where def = GameDB def def

instance NFData GameDB where
	rnf (GameDB gs ns) = rnf gs `seq` rnf ns

-- | Add a game to the database.
addGame :: GameRecord -> GameDB -> GameDB
addGame r = execState $ do
	blueNodes   <- traverse findPlayer . S.toList . blue   $ r
	orangeNodes <- traverse findPlayer . S.toList . orange $ r
	[gameNode]  <- gets (G.newNodes 1 . games)
	onGames (G.insNode (gameNode, Right r))
	traverse_ (addEdge Blue   gameNode)   blueNodes
	traverse_ (addEdge Orange gameNode) orangeNodes
	where
	onGames f = modify (\db -> db { games = f (games db) })

	findPlayer name = state $ \db@(GameDB gs ns) -> case M.lookup name ns of
		Nothing -> (node, GameDB gs' ns') where
			[node] = G.newNodes 1 gs
			ns' = M.insert name node ns
			gs' = G.insNode (node, Left name) gs
		Just node -> (node, db)

	addEdge dir gameNode playerNode
		| winner r == dir = onGames (G.insEdge (playerNode, gameNode, ()))
		| otherwise       = onGames (G.insEdge (gameNode, playerNode, ()))

-- | List all players, ordered by how many games they have played.
listPlayers :: GameDB -> [Name]
listPlayers db = map snd . sort $
	[ (negate $ G.deg (games db) node, name)
	| (name, node) <- M.toList (nodes db)
	]

-- | List all games, in no particular order (e.g. for serialization).
listGames :: GameDB -> [GameRecord]
listGames = snd . partitionEithers . map snd . G.labNodes . games

-- | List all games played by a given player.
playerGames :: GameDB -> Name -> [GameRecord]
playerGames GameDB { games = gs, nodes = ns } n = case M.lookup n ns of
	Nothing -> []
	Just playerNode ->
		[ r
		| gameNode <- G.neighbors gs playerNode
		, Just (Right r) <- [G.lab gs gameNode]
		]

-- | A maximal collection of players where all of the following hold:
--
-- 1. Each pair of players in the collection has a winning path connecting
--    them.
-- 2. Each pair of players in the collection has a losing path connecting them.
-- 3. Some tie game involves only players from the collection.
type Component = Set Name

-- | Retrieve the collections of people it makes sense to rate against each
-- other.
components :: GameDB -> [Component]
components GameDB { games = gs } =
	[ S.fromList names
	| scc_ <- G.scc gs
	, let scc = map (fromJust . G.lab gs) scc_
	      (names, games) = partitionEithers scc
	, any ((Tie==) . winner) games
	]

-- | Retrieve all games in which at least one of the players is in the given
-- component. Internal only.
laxComponentGames :: GameDB -> Set Name -> [GameRecord]
laxComponentGames db ns =
	[ game
	| gameNode <- IS.toList . IS.fromList $
		[ gameNode
		| n <- S.toList ns
		, Just playerNode <- [M.lookup n (nodes db)]
		, gameNode <- G.neighbors (games db) playerNode
		]
	, Just (Right game) <- [G.lab (games db) gameNode]
	]

-- | Retrieve all games in which all players were in the given component.
componentGames :: GameDB -> Component -> [GameRecord]
componentGames db ns = filter
	(\g -> S.isSubsetOf (blue g) ns && S.isSubsetOf (orange g) ns)
	(laxComponentGames db ns)

-- | Find games where the winners are all drawn from the first set, and the
-- losers all drawn from the second.
edgesBetween :: GameDB -> Set Name -> Set Name -> [GameRecord]
edgesBetween db ns ns' =
	[ game
	| gameNode <- IS.toList . IS.fromList $
		[ gameNode
		| n <- S.toList ns
		, Just playerNode <- [M.lookup n (nodes db)]
		, gameNode <- G.suc (games db) playerNode
		]
	, Just (Right game) <- [G.lab (games db) gameNode]
	, case winner game of
		Blue   -> blue   game `S.isSubsetOf` ns && orange game `S.isSubsetOf` ns'
		Tie    -> False
		Orange -> orange game `S.isSubsetOf` ns && blue   game `S.isSubsetOf` ns'
	]
