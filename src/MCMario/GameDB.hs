module MCMario.GameDB
	( Name
	, Speed(..)
	, PlayerSettings(..)
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
import Data.Default
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
import qualified Data.Map as M
import qualified Data.Set as S

-- | A player's name.
type Name = Text

-- | The speed pills drop.
data Speed = Low | Medium | High
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance NFData Speed where rnf x = x `seq` ()

-- | A summary of how a match was set up for one of the two players involved.
data PlayerSettings = PlayerSettings
	{ name :: Name
	, level :: Integer
	, speed :: Speed
	}
	deriving (Eq, Ord, Read, Show)

instance NFData PlayerSettings where
	rnf (PlayerSettings n l s) = rnf n `seq` rnf l `seq` rnf s

-- | The settings used to play a match, along with who won.
data GameRecord = GameRecord
	{ winner, loser :: PlayerSettings
	, date :: UTCTime
	} deriving (Eq, Ord, Read, Show)

instance NFData GameRecord where
	rnf (GameRecord w l d) = rnf w `seq` rnf l `seq` rnf d

-- Invariant: if node a points to node b with label e, then both of these hold:
-- 	name (winner e) = a
-- 	name (loser  e) = b

-- | A record of all games played.
data GameDB = GameDB
	{ games :: Gr Name GameRecord
	, nodes :: Map Name Node
	} deriving (Eq, Read, Show)

instance Default (Gr a b) where def = G.empty
instance Default GameDB where def = GameDB def def

instance NFData GameDB where
	rnf (GameDB gs ns) = rnf gs `seq` rnf ns

-- | Add a game to the database.
addGame :: GameRecord -> GameDB -> GameDB
addGame r db = GameDB (G.insEdge (src, dst, r) gs) ns where
	(db' , src) = findPlayer (name . winner $ r) db
	(db'', dst) = findPlayer (name . loser  $ r) db'
	GameDB gs ns = db''

	findPlayer name db = case M.lookup name (nodes db) of
		Nothing -> (GameDB gs' ns', node) where
			GameDB gs ns = db
			[node] = G.newNodes 1 gs
			ns' = M.insert name node ns
			gs' = G.insNode (node, name) gs
		Just node -> (db, node)

-- | List all players, ordered by how many games they have played.
listPlayers :: GameDB -> [Name]
listPlayers GameDB { games = gs } = id
	. map snd
	. sort
	. map (\(node, name) -> (negate . G.deg gs $ node, name))
	. G.labNodes
	$ gs

-- | List all games, in no particular order (e.g. for serialization).
listGames :: GameDB -> [GameRecord]
listGames = edges . games

-- | List all games played by a given player.
playerGames :: GameDB -> Name -> [GameRecord]
playerGames GameDB { games = gs, nodes = ns } n = case M.lookup n ns of
	Nothing   -> []
	Just node -> [r | (r, _) <- G.lneighbors gs node]

-- | A strongly connected component in the game graph (where nodes are players
-- and edges point from winner to loser). That is, a maximal collection of
-- players where each pair of players in the collection has a winning path and
-- a losing path connecting them.
type Component = Set Name

-- | Retrieve the collections of people it makes sense to rate against each
-- other.
components :: GameDB -> [Component]
components GameDB { games = gs } = map (S.fromList . map (fromJust . G.lab gs)) . G.scc $ gs

-- | Retrieve all games played by a specific set of people. So named because
-- the most sensible thing to call this on is a strongly connected component
-- (see 'components').
componentGames :: GameDB -> Component -> [GameRecord]
componentGames db ns = id
	. edges
	. G.labfilter (`S.member` ns)
	. games
	$ db

-- | Find edges from one component to another (in either direction).
edgesBetween :: GameDB -> Component -> Component -> [GameRecord]
edgesBetween db ns ns' =
	[ r
	-- Externally, componentGames is supposed to be called on a strongly
	-- connected component. Here we violate this rule; but it should be safe
	-- with the current implementation.
	| r <- componentGames db (ns `S.union` ns')
	, name (winner r) `S.member` ns /= name (loser r) `S.member` ns
	]

-- | Get all the edge labels from a graph. You'd think fgl would have this
-- somewhere, but I couldn't find it (and neither could Hoogle).
edges :: Graph gr => gr n e -> [e]
edges = map (\(_,_,e) -> e) . G.labEdges
