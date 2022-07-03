module Main where

import Play (playGame)
import Solve (playSolver)
import System.Environment

main :: IO ()
main = do
  f <- getArgs
  if not (null f)
    then do
      let modo = head f
      case modo of
        "mentemaestra" -> playGame
        "descifrador" -> playSolver
        _ -> do
          putStrLn "ERROR, Mal Inicio de Programa."
          putStrLn "El programa se usa: Wordle [mentemaestra/descifrador]"
    else do
      putStrLn "ERROR, Mal Inicio de Programa."
      putStrLn "El programa se usa: Wordle [mentemaestra/descifrador]"
