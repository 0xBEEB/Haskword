{- wordscore.hs
 - by Thomas Schreiber
 - 
 - Takes a list of strings and scores the intersection value of the words.
-}

module Crossword where

import System.Random
import WordScore

type Grid = [String]

emptyGrid :: Int -> Int -> Grid
emptyGrid  x y = replicate y [ s | s <- (replicate x '#') ] 

insertAWord :: Grid -> String -> Grid
insertAWord g s = (s ++ (drop (length s)) (head g)) : (tail g)
