module Example where

import WordList
import WordSearch

words1 = ["HASKELL", "CURRY", "FUNCTIONAL", "ERLANG", "LISP", "LIST", "MONAD", "FUNCTOR", "SCHEME"]
cwWords = ["BOOK", "MOLAR", "BEES", "HOUSE"]
words2 = ["ASCII", "BANDWIDTH", "BIOS", "BUG", "CACHE", "CHIP", "CPU", "CRASH", "CYBERSPACE", "DATABASE", "DESKTOP", "DISK", "DOWNLOAD", "DRIVER", "ETHERNET", "FIREWIRE", "GIGABYTE", "HACKER", "HARDWARE", "INPUT", "JAVA", "JPEG", "KEYBOARD", "LINUX", "MODEM", "PASSWORD", "PERIPHERAL", "PORT", "REBOOT", "RESOLUTION", "SAVE", "SCROLL", "SERVER", "SOUNDCARD", "TERAFLOP", "TROJANHORSE", "UPLOAD", "USB", "USERNAME", "VIRUS", "WEBBROWSER", "WINDOWS", "ZIPFILE"]

printGrid = displayGrid (insertWords (emptyGrid 10 10) (prepare words1)) words1

printGrid2 = displayGrid (insertWords (emptyGrid 22 22) (prepare words2 )) words2

printCW = displayGrid (insertWords (emptyGrid 7 7) (prepare cwWords)) cwWords
