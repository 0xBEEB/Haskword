{- wordscore.hs
 - by Thomas Schreiber
 - 
 - Takes a list of strings and scores the intersection value of the words.
-}

module Crossword where

import System.Random
import WordScore

type Grid    = [String]
type Point   = (Int, Int)
type IOPoint = (IO Int, IO Int)

emptyGrid     :: Int -> Int -> Grid
emptyGrid  x y = replicate y [ s | s <- (replicate x '#') ] 

insertAWord          :: Grid -> String -> Point -> Grid
insertAWord g s (x,y) = take y g ++ ((take x (g !! y)) 
                                 ++ s 
                                 ++ (drop (length s + x)) (g !! y)) 
                                  : (drop y g)

insertDWord           :: Grid -> String -> Point -> Grid
insertDWord g s (x,y) = take y g 
                        ++ (map (\c -> (take x (g !! snd c)) 
                        ++ (fst c) : drop (x + 1) (g !! snd c)) (zip s [y..])) 
                        ++ drop (length s + y) g

insertSWWord           :: Grid -> String -> Point -> Grid
insertSWWord g s (x,y) = take y g 
                        ++ (map (\c -> (take (x + snd c) (g !! snd c)) 
                           ++ (fst c) : drop (x + (snd c) + 1) (g !! snd c)) 
                           (zip s [y..])) 
                        ++ drop (length s + y) g

randomPoint  :: Grid -> IOPoint 
randomPoint g = (randomRIO (0, 1 - length (head g)),
                 randomRIO (0, 1 - length g))

getCenter  :: Grid -> Point
getCenter g = ((length (head g) - 1) `div` 2, (length g - 1)`div` 2)

