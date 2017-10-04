module Main where

import Control.DeepSeq
import Criterion.Main
import Data.Default
import MCMario.CSV
import MCMario.GameDB
import MCMario.RatingDB

main = do
	gdb <- load "bench.csv"
	let gameRecord = head (components gdb >>= componentGames gdb)
	    rdb = improveRatings gdb def !! 3
	-- make sure the various components are ready
	(gdb, gameRecord, rdb) `deepseq` return ()

	defaultMain $
		[ bench "0 rating updates" $
			nf (\gdb -> improveRatings gdb def !!  0) gdb
		, bench "0 rating updates, already updated" $
			nf (\gdb -> improveRatings gdb rdb !!  0) gdb
		, bench "0 game additions" $
			nf id gdb
		, bench "1 game addition" $
			nf (\gdb -> addGame gameRecord gdb) gdb
		, bench "0 rating updates, 1 game addition" $
			nf (\gdb -> improveRatings (addGame gameRecord gdb) rdb !! 0) gdb
		, bench "1 rating update" $
			nf (\gdb -> improveRatings gdb def !!  1) gdb
		, bench "listPlayers" $
			nf listPlayers gdb
		, bench "10 rating updates" $
			nf (\gdb -> improveRatings gdb def !! 10) gdb
		]
