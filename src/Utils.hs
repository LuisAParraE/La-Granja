module Utils where

import GHC.IO.Handle (hClose, hGetLine, hIsEOF)
import System.IO (IOMode (ReadMode), openFile)

turnos :: Int
turnos = 6

palabrasPath :: [Char]
palabrasPath = "Palabras.txt"

loadWords :: IO [String]
loadWords = do
  handler <- openFile palabrasPath ReadMode
  palabras <- wordByWord handler []
  hClose handler
  return palabras

wordByWord handler wordList = do
  hasLine <- hIsEOF handler
  if not hasLine
    then do
      newWord <- hGetLine handler
      wordByWord handler (wordList ++ [newWord])
    else do
      return wordList
