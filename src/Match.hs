{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Match
  ( Match (..),
    checkGuess,
    lectura,
  )
where

import Text.ParserCombinators.ReadPrec
import Text.Read

-- | Tipo de dato Match que se encarga de Clasificar las letras en Toro, Vaca, Nada
data Match
  = Nada
  | Vaca
  | Toro
  deriving (Eq, Ord)

-- | Instancia Read para el tipo de data MATCH
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

-- | Instancia Show para el tipo de data MATCH
instance Show Match where
  show Toro = "T"
  show Vaca = "V"
  show Nada = "-"

-- | Funciones para llamar todo el proceso de verificación de la adivinación. Crea dos tipos de
-- arreglos de pares (Char,Match) y (Char,Bool) respectivamente.
checkGuess ::
  -- | Palabra que es la adivinación del usuario.
  [Char] ->
  -- | Palabra elegida por el juego que el usuario debe adivinar
  [Char] ->
  -- | Arrglo de vaca,toros o guiones para saber si se adivino o no la palabra
  [Match]
checkGuess [] _ = []
checkGuess xs ys = lookToros (zip xs (repeat Nada)) (zip ys (repeat False)) [] []

-- | Función que se encarga de verificar los toros de la adivinación. Recibe 4 argumentos
-- arreglos de pares (Char,Match), (Char,Bool) y dos arreglos vacios que se llenaran mientras hace la recursión.
-- Si una letra de la dupla de Match en la posición i-esima coincide con su homologo de  la dupla de Bool,
-- se guarda en el primer arreglo vació la convinación (Char,Toro) y en el segundo arreglo vacio
-- (Char,True), para saber que esa letra esta en buena posición y que la primera letra del objetivo ya fue usada.
lookToros ::
  -- | Dupla de las letras de la adivinación con Nada
  [(Char, Match)] ->
  -- | Dupla del objetivo a adivinar con False
  [(Char, Bool)] ->
  -- | Dupla de las letras de la adivinación, con Toro si es que coincidio con el objetivo
  [(Char, Match)] ->
  -- | Dupla del objetivo a adivinar con True, si es que coincidio con la adivinación
  [(Char, Bool)] ->
  -- | Arreglo de Match que dicen que tan cerca esta la adivinación del Objetivo
  [Match]
lookToros [] _ xs zs = lookVacas xs zs
lookToros ((c, cs) : ys) ((t, ts) : ws) xs zs
  | c == t && not ts = lookToros ys ws (xs ++ [(c, Toro)]) (zs ++ [(t, True)])
  | otherwise = lookToros ys ws (xs ++ [(c, cs)]) (zs ++ [(t, ts)])

-- | Esta función se encarga de encontrar las vacas entre la adivinación y el objetivo.
-- Recibe dos arreglos de duplas, uno de (Char,Match) y otro de (Char,Bool) respectivamente.
-- Procede a comparar cada char del primer arreglo con cada char del segundo.
lookVacas [] _ = []
lookVacas ((x, y) : xs) ((t, ts) : (a, as) : (r, rs) : (g, gs) : (e, es) : ws)
  | x == a && not as && y == Nada = Vaca : lookVacas xs ((a, True) : (r, rs) : (g, gs) : (e, es) : (t, ts) : ws)
  | x == r && not rs && y == Nada = Vaca : lookVacas xs ((a, as) : (r, True) : (g, gs) : (e, es) : (t, ts) : ws)
  | x == g && not gs && y == Nada = Vaca : lookVacas xs ((a, as) : (r, rs) : (g, True) : (e, es) : (t, ts) : ws)
  | x == e && not es && y == Nada = Vaca : lookVacas xs ((a, as) : (r, rs) : (g, gs) : (e, True) : (t, ts) : ws)
  | otherwise = y : lookVacas xs ((a, as) : (r, rs) : (g, gs) : (e, es) : (t, ts) : ws)

-- | Función auxiliar para poder leer un arreglo de match
lectura :: [Char] -> [Match]
lectura = map (\x -> read [x])