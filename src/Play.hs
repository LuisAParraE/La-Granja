{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Play where

import Data.Char (toUpper)
import Match (Match (..), checkGuess)
import System.IO
import Text.ParserCombinators.ReadP (char)
import Text.Read
import Utils (loadWords, randomSelect, turnos)

-- | Función que inicia el juego, carga todas las palabras, y elige una al azar.
playGame :: IO ()
playGame = do
  allWords <- loadWords
  palabra <- randomSelect allWords
  gameStatus palabra 0

-- | El ciclo del juego, llama a las funciones de verificación de la adivinación y se encarga de terminar el juego, de acertar
-- la palabra o despues de los turnos estipulados.
gameStatus ::
  -- | Palabra seleccionada a Azar
  [Char] ->
  -- | Numero de turnos que lleva el juego
  Int ->
  -- | No retorna nada. Ya que todo se maneja con prints
  IO ()
gameStatus palabra turn
  | turn < turnos = do
    adivinacion <- leerEntrada
    let guess = map toUpper adivinacion
    let comparacion = checkGuess guess palabra
    putStr "MENTEMAESTRA:"
    hFlush stdout
    traverse (putStr . show) comparacion
    if elem Vaca comparacion || elem Nada comparacion
      then do
        putStrLn ""
        gameStatus palabra (turn + 1)
      else do
        putStrLn "Ganaste!!!"
        hFlush stdout
  | turn >= turnos = do
    putStrLn ""
    putStr "La palabra era "
    hFlush stdout
    putStrLn palabra
    hFlush stdout

-- | Función Dedicada a leer la entrada del usuario, manteniendo el ciclo hasta que de una entrada Correcta.
leerEntrada :: IO String
leerEntrada = do
  putStr "DECIFRADOR:"
  hFlush stdout
  entrada <- getLine
  if length entrada /= 5
    then leerEntrada
    else do
      return entrada
