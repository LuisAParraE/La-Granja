{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Play
  ( playGame,
  )
where

import Data.Char (toUpper)
import Match (Match (..), checkGuess)
import System.IO
import Text.ParserCombinators.ReadP (char)
import Text.Read
import Utils (loadWords, randomSelect, turnos)

playGame = do
  allWords <- loadWords
  palabra <- randomSelect allWords
  gameStatus palabra 0

gameStatus :: [Char] -> Int -> IO ()
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

--leerEntrada :: IO [Char]
leerEntrada :: IO String
leerEntrada = do
  putStr "DECIFRADOR:"
  hFlush stdout
  entrada <- getLine
  if length entrada /= 5
    then leerEntrada
    else do
      return entrada
