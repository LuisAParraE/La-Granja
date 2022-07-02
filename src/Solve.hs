module Solve where

import Match (Match (..), checkGuess, lectura)
import Text.Read

leerEntradaSolver :: IO [Match]
leerEntradaSolver = do
  putStr "DECIFRADOR:"
  entrada <- getLine
  let adivinacion = lectura entrada
  if length entrada /= 5
    then leerEntradaSolver
    else do
      return adivinacion