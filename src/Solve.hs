{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solve where

import Match (Match (..), checkGuess, lectura)
import Text.Read
import Utils (turnos)

solveTheGame palabra turn
  | turn == 0 = do
    adivinacion <- getLine
    putStr "La palabra sugerida es "
    putStrLn adivinacion
    codigo <- leerEntradaSolver
    if foldr ((&&) . (== Toro)) True codigo
      then do
        putStrLn ""
        putStrLn "Has Ganado!!!"
      else do
        nextPalabra <- getLine
        solveTheGame nextPalabra (turn + 1)
  | turn < turnos = do
    putStr "La palabra sugerida es "
    putStrLn palabra
    codigo <- leerEntradaSolver
    if foldr ((&&) . (== Toro)) True codigo
      then do
        putStrLn ""
        putStrLn "Has Ganado!!!"
      else do
        nextPalabra <- getLine
        solveTheGame nextPalabra (turn + 1)
  | turn >= turnos = do
    putStrLn ""
    putStrLn "Has Perdido!!! :c"

leerEntradaSolver :: IO [Match]
leerEntradaSolver = do
  putStr "PISTA:"
  entrada <- getLine
  let adivinacion = lectura entrada
  if length entrada /= 5
    then leerEntradaSolver
    else do
      return adivinacion