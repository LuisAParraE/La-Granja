module Utils
  ( turnos,
    loadWords,
    randomSelect,
  )
where

import GHC.IO.Handle (hClose, hGetLine, hIsEOF)
import System.IO (IOMode (ReadMode), openFile)
import System.Random (randomRIO)

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

randomSelect lista = do
  num <- randomRIO (0, length lista -1)
  return $ lista !! num
