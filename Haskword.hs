{- Haskword
 - Copyright 2012 Thomas Schreiber <ubiquill@gmai.com>
 -
 - Haskword is a word search generator written in Haskell.
 -}

module Main where

import WordList
import WordSearch
import PuzzlePrinter

puzzle        :: FilePath -> String -> Int -> Int -> IO ()
puzzle f s x y = do ws <- wordList f
                    writePuzzle s grid (snd ws)
                    where grid = do ws <- wordList f
                                    (getPuzzle
                                      (insertWords (emptyGrid x y) (fst ws))
                                      (fst ws))

puzzle1 = puzzle "puzzle1.txt" "puzzle1.tex" 10 10
puzzle2 = puzzle "puzzle2.txt" "puzzle2.tex" 20 20

main = do puzzle1
          puzzle2
          return ()
