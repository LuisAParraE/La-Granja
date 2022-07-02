{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Play where

import Match (Match (..), checkGuess)
import Text.ParserCombinators.ReadP (char)
import Text.Read

turnos :: Int
turnos = 6

--playGame = do
--    let word = "cazar"

gameStatus :: [Char] -> Int -> IO ()
gameStatus palabra turn
  | turn < turnos = do
    adivinacion <- leerEntrada
    let comparacion = checkGuess adivinacion palabra
    putStr "MENTEMAESTRA:"
    traverse (putStr . show) comparacion
    if elem Vaca comparacion || elem Nada comparacion
      then do
        putStrLn ""
        gameStatus palabra (turn + 1)
      else putStrLn "Ganaste!!!"
  | turn >= turnos = do
    putStrLn ""
    putStr "La palabra era "
    putStrLn palabra

--leerEntrada :: IO [Char]
leerEntrada :: IO String
leerEntrada = do
  putStr "DECIFRADOR:"
  entrada <- getLine
  if length entrada /= 5
    then leerEntrada
    else do
      return entrada
