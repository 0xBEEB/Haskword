{- wordscore.hs
 - by Thomas Schreiber
 - 
 - Takes a list of strings and scores the intersection value of the words.
-}

module WordSearch where

import System.Random

type Grid  = [String]
type Point = (Int, Int)

emptyGrid     :: Int -> Int -> Grid
emptyGrid  x y = replicate y [ s | s <- (replicate x '#') ] 

pick   :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)


displayGrid     :: IO Grid -> [String] -> IO ()
displayGrid g ws = do grid <- g
                      filledGrid <- fillGrid ws grid
                      putStr (unlines filledGrid)
                      putStrLn ("+++++")
                      mapM (\w -> putStr (w ++ "    ")) ws
                      putStrLn ""
                      putStrLn ("+++++")
                      putStr (unlines grid)

replaceHash     :: [String] -> Char -> IO Char
replaceHash ws c = if c == '#' then do r <- pick (concat ws)
                                       return r
                               else return c

fillLine     :: [String] -> String -> IO String
fillLine ws l = mapM (replaceHash ws) l

fillGrid     :: [String] -> Grid -> IO Grid
fillGrid ws g = mapM (fillLine ws) g

waysToInsert = [(fitsA, insertAWord), (fitsAR, insertAWordR),
                (fitsD, insertDWord), (fitsDR, insertDWordR),
                (fitsSW1, insertSWWord1), (fitsSW1R, insertSWWord1R),
                (fitsSW2, insertSWWord2), (fitsSW2R, insertSWWord2R)]

randomWay = pick waysToInsert

insertWord       :: Grid -> String -> IO Grid
insertWord grid w = do method <- randomWay
                       x <- randomRIO (0, length (head grid) - 1)
                       y <- randomRIO (0, length grid - 1)
                       if (fst method) grid w (x,y)
                         then return $ ((snd method) grid w (x,y))
                         else insertWord grid w

insertWords         :: Grid -> [String] -> IO Grid
insertWords g []     = return g
insertWords g (x:xs) = do newGrid <- insertWord g x
                          insertWords newGrid xs

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
