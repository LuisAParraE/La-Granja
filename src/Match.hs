{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Match
  ( Match (..),
    checkGuess,
    lectura,
  )
where

import Text.ParserCombinators.ReadPrec
import Text.Read

data Match
  = Nada
  | Vaca
  | Toro
  deriving (Eq, Ord)

instance Read Match where
  readPrec =
    parens
      ( do
          g <- get
          if g == 'V'
            then pure Vaca
            else
              if g == 'T'
                then pure Toro
                else
                  if g == '-'
                    then pure Nada
                    else pfail
      )

instance Show Match where
  show Toro = "T"
  show Vaca = "V"
  show Nada = "-"

--Funciones para verificar si coinciden las palabras
checkGuess :: [Char] -> [Char] -> [Match]
checkGuess [] _ = []
checkGuess xs ys = lookToros (zip xs (repeat Nada)) (zip ys (repeat False)) [] []

lookToros :: [(Char, Match)] -> [(Char, Bool)] -> [(Char, Match)] -> [(Char, Bool)] -> [Match]
lookToros [] _ xs zs = lookVacas xs zs
lookToros ((c, cs) : ys) ((t, ts) : ws) xs zs
  | c == t && not ts = lookToros ys ws (xs ++ [(c, Toro)]) (zs ++ [(t, True)])
  | otherwise = lookToros ys ws (xs ++ [(c, cs)]) (zs ++ [(t, ts)])

lookVacas [] _ = []
lookVacas ((x, y) : xs) ((t, ts) : (a, as) : (r, rs) : (g, gs) : (e, es) : ws)
  | x == a && not as = Vaca : lookVacas xs ((a, True) : (r, rs) : (g, gs) : (e, es) : (t, ts) : ws)
  | x == r && not rs = Vaca : lookVacas xs ((a, as) : (r, True) : (g, gs) : (e, es) : (t, ts) : ws)
  | x == g && not gs = Vaca : lookVacas xs ((a, as) : (r, rs) : (g, True) : (e, es) : (t, ts) : ws)
  | x == e && not es = Vaca : lookVacas xs ((a, as) : (r, rs) : (g, gs) : (e, True) : (t, ts) : ws)
  | otherwise = y : lookVacas xs ((a, as) : (r, rs) : (g, gs) : (e, es) : (t, ts) : ws)

lectura :: [Char] -> [Match]
lectura = map (\x -> read [x])