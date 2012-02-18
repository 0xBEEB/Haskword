{- wordscore.hs
 - by Thomas Schreiber
 - 
 - Takes a list of strings and scores the intersection value of the words.
-}

module WordScore where

import Data.List (sortBy)
import Data.Ord (comparing)

scoreWord          :: String -> String -> Integer
scoreWord [] ss     = 0
scoreWord (c:cs) ss = toInteger (length (filter (== c) ss)) + scoreWord cs ss

listScore :: [String] -> [Integer]
listScore ss = map (\x -> (scoreWord x (concat ss)) - (scoreWord x x)) ss

rankWords    :: [String] -> [String]
rankWords  ss = reverse $ map snd $ 
                  sortBy (comparing fst) (zip (listScore ss) ss)
