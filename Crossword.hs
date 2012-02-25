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

fitsA          :: Grid -> String -> Point -> Bool
fitsA g s (x,y) = foldr (&&) True (map (\c -> fst c == snd c || snd c == '#') 
                        (zip s (take (length s) (drop x (g !! y)))))

insertAWordR          :: Grid -> String -> Point -> Grid
insertAWordR g s (x,y) = insertAWord g (reverse s) (x,y)

fitsAR          :: Grid -> String -> Point -> Bool
fitsAR g s (x,y) = fitsA g (reverse s) (x,y)        

insertDWord          :: Grid -> String -> Point -> Grid
insertDWord g s (x,y) = take y g 
                        ++ (map (\c -> (take x (g !! snd c)) 
                        ++ (fst c) : drop (x + 1) (g !! snd c)) (zip s [y..])) 
                        ++ drop (length s + y) g

fitsD          :: Grid -> String -> Point -> Bool
fitsD g s (x,y) = foldr (&&) True (map (\c -> fst c == snd c || snd c == '#') 
                        (zip s (map (!!x) (take (length s) (drop y g)))))

insertDWordR          :: Grid -> String -> Point -> Grid
insertDWordR g s (x,y) = insertDWord g (reverse s) (x,y)

fitsDR          :: Grid -> String -> Point -> Bool
fitsDR g s (x,y) = fitsD g (reverse s) (x,y)        

insertSWWord1          :: Grid -> String -> Point -> Grid
insertSWWord1 g s (x,y) = take y g 
                        ++ (map (\c -> (take (x + snd c - y) (g !! snd c)) 
                           ++ (fst c) : drop (x + (snd c) + 1 - y) (g !! snd c)) 
                           (zip s [y..])) 
                        ++ drop (length s + y) g

fitsSW1          :: Grid -> String -> Point -> Bool
fitsSW1 g s (x,y) = foldr (&&) True (map (\c -> fst c == snd c || snd c == '#') 
                        (zip s (map 
                                (\c -> (g !! snd c) !! (x + (snd c) - y)) 
                                (zip s [y..]))))

insertSWWord1R          :: Grid -> String -> Point -> Grid
insertSWWord1R g s (x,y) = insertSWWord1 g (reverse s) (x,y)                  

fitsSW1R          :: Grid -> String -> Point -> Bool
fitsSW1R g s (x,y) = fitsSW1 g (reverse s) (x,y)

insertSWWord2          :: Grid -> String -> Point -> Grid
insertSWWord2 g s (x,y) = insertSWWord1 (reverse (map reverse g)) s (x,y)

fitsSW2          :: Grid -> String -> Point -> Bool
fitsSW2 g s (x,y) = fitsSW1 (reverse (map reverse g)) s (x,y)

insertSWWord2R          :: Grid -> String -> Point -> Grid
insertSWWord2R g s (x,y) = insertSWWord1 g (reverse s) (x,y)                  

fitsSW2R          :: Grid -> String -> Point -> Bool
fitsSW2R g s (x,y) = fitsSW2 g (reverse s) (x,y)

randomPoint  :: Grid -> IOPoint 
randomPoint g = (randomRIO (0, 1 - length (head g)),
                 randomRIO (0, 1 - length g))

getCenter  :: Grid -> Point
getCenter g = ((length (head g) - 1) `div` 2, (length g - 1)`div` 2)

