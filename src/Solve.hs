{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Solve
  ( playSolver,
  )
where

import Match (Match (..), checkGuess, lectura)
import Minimax
import System.IO
import Text.Read
import Utils (loadWords, randomSelect, turnos)

-- | Función a Exportar y que Inicia el estado del SOLVER
-- Carga todas las palabras en una lista, setea el turno en 0 y elige una palabra al azar para iniciar
playSolver :: IO ()
playSolver = do
  allWords <- loadWords
  palabra <- randomSelect allWords
  solveTheGame palabra 0 allWords []

-- | Función que maneja el flujo del solver, muestra la palabra sugerida, y luego pide la pista.
-- Si la pista no es completamente buena, procede a buscar una palabra a sugerir y se repite el proceso.
-- Si se llega a más 6 turnos. Finaliza el programa.
solveTheGame palabra turn allWords usedWords
  | turn < turnos = do
    putStr "La palabra sugerida es "
    hFlush stdout
    putStrLn palabra
    hFlush stdout
    codigo <- leerEntradaSolver
    if foldr ((&&) . (== Toro)) True codigo
      then do
        putStrLn ""
        hFlush stdout
        putStrLn "La IA ha ganado!"
        hFlush stdout
      else do
        let nextPalabra = damePalabra usedWords allWords (palabra, codigo)
        if nextPalabra == "TRAMPOSO"
          then do
            putStrLn ""
            hFlush stdout
            putStrLn "Has sido un tramposo, la IA gana! :c"
            hFlush stdout
          else do
            solveTheGame nextPalabra (turn + 1) allWords (usedWords ++ [palabra])
  | turn >= turnos = do
    putStrLn ""
    hFlush stdout
    putStrLn "La IA ha perdido! :c"
    hFlush stdout

-- | Función encargada de leer la entrada, verificar si es posible dicha entrada. Y luego convertirla en un tipo de dato Match,
-- que es el codigo utilizado para las palabras.
leerEntradaSolver :: IO [Match]
leerEntradaSolver = do
  putStr "PISTA:"
  hFlush stdout
  entrada <- getLine
  let adivinacion = lectura entrada
  if length entrada /= 5
    then leerEntradaSolver
    else do
      return adivinacion