{-# LANGUAGE LambdaCase #-}

import Data.Default
import MCMario.CSV
import MCMario.GameDB
import MCMario.RatingDB
import System.Environment
import System.Exit

import qualified Data.Map as M
import qualified Data.Text as T

defaultGamesFile :: FilePath
defaultGamesFile = "bench.csv"

usage :: IO String
usage = do
	progName <- getProgName
	return $  "USAGE: " ++ progName ++ " FILE\n"
	       ++ "Produce a FILE suitable for reading in gnuplot that graphs players' ratings vs.\n"
	       ++ "the number of ratings updates done. Reads games from " ++ defaultGamesFile ++ "."

renderRatings :: [RatingDB] -> String
renderRatings rdbs = unlines (header : zipWith renderDB [0..] rdbs) where
	header = unwords ("# iteration" : map T.unpack (M.keys (rdbs !! 1)))
	renderDB i rdb = unwords (show i : map (show . snd) (M.toAscList rdb))

main :: IO ()
main = getArgs >>= \case
	[outFile] -> loadGames defaultGamesFile >>= \case
		Right gdb -> writeFile outFile
		           . renderRatings
		           . take 1000
		           . improveComponentRatings gdb def
		           $ head (components gdb)
		Left err -> die err
	_ -> usage >>= die
