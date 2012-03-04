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

emptyIOGrid    :: Int -> Int -> IO Grid
emptyIOGrid x y = do return (emptyGrid x y)

displayGrid  :: IO Grid -> IO ()
displayGrid g = do grid <- g
                   putStr (unlines grid)

insertWord    :: IO Grid -> String -> IO Grid
insertWord g w = do method <- randomWay
                    grid <- g
                    x <- randomRIO (0, length (head grid) - 1)
                    y <- randomRIO (0, length grid - 1)
                    if (fst method) grid w (x,y)
                      then return $ ((snd method) grid w (x,y))
                      else insertWord g w

insertWords         :: IO Grid -> [String] -> IO Grid
insertWords g []     = g
insertWords g (x:xs) = insertWords (insertWord g x) xs

waysToInsert = [(fitsA, insertAWord), (fitsAR, insertAWordR),
                (fitsD, insertDWord), (fitsDR, insertDWordR),
                (fitsSW1, insertSWWord1), (fitsSW1R, insertSWWord1R),
                (fitsSW2, insertSWWord2), (fitsSW2R, insertSWWord2R)]

pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

randomWay = pick waysToInsert

insertAWord          :: Grid -> String -> Point -> Grid
insertAWord g s (x,y) = take y g ++ ((take x (g !! y)) 
                                 ++ s 
                                 ++ (drop (length s + x)) (g !! y)) 
                                  : (drop (y+1) g)

fitsA          :: Grid -> String -> Point -> Bool
fitsA g s (x,y) = if (x + length s) <= (length . head) g then
                  foldr (&&) True (map (\c -> fst c == snd c || snd c == '#') 
                        (zip s (take (length s) (drop x (g !! y)))))
                  else False

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
fitsD g s (x,y) = if (y + length s) <= length g then
                  foldr (&&) True (map (\c -> fst c == snd c || snd c == '#') 
                        (zip s (map (!!x) (take (length s) (drop y g)))))
                  else False

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
fitsSW1 g s (x,y) = if (x + length s) <= (length . head) g 
                    && (y + length s) <= length g then 
                    foldr (&&) True (map (\c -> fst c == snd c || snd c == '#') 
                        (zip s (map 
                                (\c -> (g !! snd c) !! (x + (snd c) - y)) 
                                (zip s [y..]))))
                    else False

insertSWWord1R          :: Grid -> String -> Point -> Grid
insertSWWord1R g s (x,y) = insertSWWord1 g (reverse s) (x,y)                  

fitsSW1R          :: Grid -> String -> Point -> Bool
fitsSW1R g s (x,y) = fitsSW1 g (reverse s) (x,y)

insertSWWord2          :: Grid -> String -> Point -> Grid
insertSWWord2 g s (x,y) = reverse (insertSWWord1 (reverse  g) s (x,y))

fitsSW2          :: Grid -> String -> Point -> Bool
fitsSW2 g s (x,y) = fitsSW1 (reverse g) s (x,y)

insertSWWord2R          :: Grid -> String -> Point -> Grid
insertSWWord2R g s (x,y) = insertSWWord2 g (reverse s) (x,y)                  

fitsSW2R          :: Grid -> String -> Point -> Bool
fitsSW2R g s (x,y) = fitsSW2 g (reverse s) (x,y)

randomPoint  :: Grid -> IOPoint 
randomPoint g = (randomRIO (0, 1 - length (head g)),
                 randomRIO (0, 1 - length g))

getCenter  :: Grid -> Point
getCenter g = ((length (head g) - 1) `div` 2, (length g - 1)`div` 2)

