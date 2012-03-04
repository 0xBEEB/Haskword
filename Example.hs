module Example where

import WordScore
import Crossword



printGrid = displayGrid (insertWords (emptyIOGrid 14 14) (rankWords ["HASKELL", "CURRY", "FUNCTIONAL", "ERLANG", "LISP", "LIST", "MONAD", "FUNCTOR", "SCHEME"]))
