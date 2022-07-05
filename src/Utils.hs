module Utils
  ( turnos,
    loadWords,
    randomSelect,
  )
where

import GHC.IO.Handle (hClose, hGetLine, hIsEOF)
import System.IO (IOMode (ReadMode), openFile)
import System.Random (randomRIO)

-- | Funcion con el numero de turnos que durara el juego.
turnos :: Int
turnos = 6

-- | Funcion con el nombre del archivo que queremos abrir.
palabrasPath :: [Char]
palabrasPath = "Palabras.txt"

-- | Fución encargada de cargar todas las palabras de un archivo en una lista
loadWords :: IO [String]
loadWords = do
  handler <- openFile palabrasPath ReadMode
  palabras <- wordByWord handler []
  hClose handler
  return palabras

-- | Función auxiliar que recibe el handler de un archivo y lo itera hasta que llega al final, cargando palabra por palabra.
wordByWord handler wordList = do
  hasLine <- hIsEOF handler
  if not hasLine
    then do
      newWord <- hGetLine handler
      wordByWord handler (wordList ++ [newWord])
    else do
      return wordList

-- | Función para seleccionar una palabra la azar de una lista
randomSelect lista = do
  num <- randomRIO (0, length lista -1)
  return $ lista !! num
