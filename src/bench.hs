module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Default
import MCMario.CSV
import MCMario.GameDB
import MCMario.RatingDB

main = do
	Right gdb <- loadGames "bench.csv"
	let gameRecord = head (components gdb >>= componentGames gdb)
	    component = head (components gdb)
	    rdb = improveComponentRatings gdb def component !! 3
	-- make sure the various components are ready
	(gdb, gameRecord, component, rdb) `deepseq` return ()

	defaultMain $
		[ bench "0 rating updates" $
			nf (\gdb -> improveComponentRatings gdb def component !!  0) gdb
		, bench "0 rating updates, already updated" $
			nf (\gdb -> improveComponentRatings gdb rdb component !!  0) gdb
		, bench "0 game additions" $
			nf id gdb
		, bench "1 game addition" $
			nf (\gdb -> addGame gameRecord gdb) gdb
		, bench "0 rating updates, 1 game addition" $
			nf (\gdb -> improveComponentRatings (addGame gameRecord gdb) rdb component !! 0) gdb
		, bench "1 rating update" $
			nf (\gdb -> improveComponentRatings gdb def component !!  1) gdb
		, bench "listPlayers" $
			nf listPlayers gdb
		, bench "10 rating updates" $
			nf (\gdb -> improveComponentRatings gdb def component !! 10) gdb
		]
