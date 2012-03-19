{- WordList.hs
 - Copyright 2012 Thomas Schreiber <ubiquill@gmail.com>
 - 
 - Takes a file containing words and creates a wordlist for adding to a word
 - search.
-}

module WordList where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (isLetter)
import Char (toUpper)
import System.IO
import Control.Monad

wordList  :: FilePath -> IO ([String], [String])
wordList f = do ws <- getWords f
                return (prepare ws, ws)

scoreWord          :: String -> String -> Integer
scoreWord [] ss     = 0
scoreWord (c:cs) ss = toInteger (length (filter (== c) ss)) + scoreWord cs ss

listScore :: [String] -> [Integer]
listScore ss = map (\x -> (scoreWord x (concat ss)) - (scoreWord x x)) ss

rankWords    :: [String] -> [String]
rankWords  ss = reverse . map snd $ 
                  sortBy (comparing fst) (zip (listScore ss) ss)

getWords :: FilePath -> IO [String]
getWords  = (liftM lines . readFile)

allCaps :: [String] -> [String]
allCaps  = (map . map) toUpper

sanatize :: [String] -> [String]
sanatize  = map (filter isLetter)

cleanWords :: [String] -> [String]
cleanWords  = allCaps . sanatize

prepare :: [String] -> [String]
prepare  = rankWords . cleanWords
