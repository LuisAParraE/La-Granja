
module Minimax where

import Match
import Data.List

type Combinacion = [Match]
type Adivinacion = (String, Combinacion)

newtype TuplaPuntuada v =
    TP (v, Float)
    deriving (Show)

instance Eq (TuplaPuntuada v) where
  (TP (_, s)) == (TP (_, s2)) = s == s2

instance Ord (TuplaPuntuada v) where
  (TP (_, s)) `compare` (TP (_, s2)) = s `compare` s2


data Arbol v =
      Hoja
    | Nodo v (Arbol v) [Arbol v]

  deriving (Show, Eq, Read)

letterValue :: Char -> Float
letterValue x
  | x == 'A' || x == 'E' = 0.1
  | x == 'I' || x == 'N' || x == 'O' || x == 'R' || x == 'S' = 0.2
  | x == 'D' || x == 'L' || x == 'C' || x == 'T' || x == 'U' = 0.3
  | x == 'B' || x == 'G' || x == 'M' || x == 'P' = 0.5
  | x == 'F' || x == 'H' || x == 'Q' || x == 'V' || x == 'Y' = 0.8
  | x == 'J' || x == 'K' || x == 'W' || x == 'X' || x == 'Z' = 1
  | otherwise = 0

matchValue :: Fractional p => Match -> p
matchValue Vaca = 0.1
matchValue Toro = 0.2
matchValue _ = 0

scoreWord :: String -> Float
scoreWord [] = 0
scoreWord xs = sum $ map letterValue xs

scoreTip :: Fractional p => Combinacion -> p
scoreTip x = 1.0 - sum (map matchValue x)


soloPalCompat :: [String] -> Adivinacion -> [String]
soloPalCompat x y = filter (\w -> palCompat w y) x

--No importar
countChar :: Char -> String -> Int
countChar x y = sum [1 | c <- y, c == x]

--No importar
--Caracter a buscar que matchea vaca en un string
-- antes era solo vacas
countCharVacasToros :: Char -> Adivinacion -> Int
countCharVacasToros x (y, z) = sum [1 | (c, v) <- zip y z, (v == Vaca || v == Toro) && c == x]


--No importar
--PRIMER STRING A PROBAR SI O SI Y SEGUNDO ADIV ANTERIOR
torosBienPal :: String -> Adivinacion -> Bool
torosBienPal x (y, z) = all (\(g,h,i) -> (i /= Toro) || (g == h)) (zip3 x y z)

--No importar
--PRIMER STRING A PROBAR SI O SI Y SEGUNDO ADIV ANTERIOR
noVacasIgual :: String -> Adivinacion -> Bool
noVacasIgual x (y, z) = all (\(g,h,i) -> (i /= Vaca) || (g /= h)) (zip3 x y z)

--No importar
--PRIMER STRING A PROBAR SI O SI Y SEGUNDO ADIV ANTERIOR
mismasVacas :: String -> Adivinacion -> Bool
mismasVacas x (y, z) = all (\(h,i) -> (i /= Vaca) || (countChar h y <= countChar h x)) (zip y z)

--No importar
--PRIMER STRING A PROBAR SI O SI Y SEGUNDO ADIV ANTERIOR
vacasBienPal :: String -> Adivinacion -> Bool
vacasBienPal x y = noVacasIgual x y && mismasVacas x y

--palAntes :: Arbol Adivinacion -> String -> Bool
--palAntes (Nodo (w, v) p _) c
--  | w == c = True
--  | otherwise = palAntes p c
--palAntes Hoja _ = False

--No importar
--PRIMER STRING A PROBAR SI O SI Y SEGUNDO ADIV ANTERIOR
ningBienPal :: String -> Adivinacion -> Bool
ningBienPal x (y, z) = all (\(h,i) -> (i /= Nada) || (countCharVacasToros h (y, z) == countChar h x)) (zip y z)

--PRIMER STRING A PROBAR SI O SI Y SEGUNDO ADIV ANTERIOR
palCompat :: String -> Adivinacion -> Bool
palCompat x (w, t) =  x /= w && torosBienPal x (w, t) && vacasBienPal x (w, t) && ningBienPal x (w, t)


allCombs :: [Combinacion]
allCombs = [[x, y, z, w, k] | x <- m, y <- m, z <- m, w <- m, k <- m]
    where m = [Vaca, Toro, Nada]

--nuevo viejo da igual
mismosToros :: Combinacion -> Combinacion -> Bool
mismosToros x t = all (\(a, b) -> (b /= Toro) || (a == b)) (zip x t)

buscaQuitaVT :: Char -> Adivinacion -> Combinacion
buscaQuitaVT c (w:ws, x:xs)
  | w /= c = x : buscaQuitaVT c (ws, xs)
  | otherwise = if x /= Nada then Nada : xs else x : buscaQuitaVT c (ws, xs)

buscaQuitaVT _ _ = []

tieneVacaToro :: Char -> Adivinacion -> (Bool, Adivinacion)
tieneVacaToro c (w, t) = (com /= t, (w, com))
  where com = buscaQuitaVT c (w, t)

-- Nueva adiv y vieja adiv
-- Cambia es la nueva adivinacion
vacasBienCom :: Adivinacion -> Adivinacion -> Bool
vacasBienCom (nw, nt) (w:ws, t:ts)
  | t /= Vaca = vacasBienCom (nw, nt) (ws, ts)
  | otherwise = res && vacasBienCom adiv (ws, ts)
    where (res, adiv) = tieneVacaToro w (nw, nt)

vacasBienCom _ _ = True

combAntes :: Arbol Adivinacion -> Combinacion -> Bool
combAntes (Nodo (w, v) p _) c
  | v == c = True
  | otherwise = combAntes p c
combAntes Hoja _ = False

--una letra que no era vaca, no puede ser vaca ahora.
--nueva adiv y vieja adiv
combCompat :: Adivinacion -> Adivinacion -> Arbol Adivinacion -> Bool
combCompat (x, y) (z, w) a = mismosToros y w && vacasBienCom (x, y) (z, w) && not (combAntes a y)

-- nueva palabra padre
crearNodosPalabra :: String -> Arbol Adivinacion -> [Arbol Adivinacion]
crearNodosPalabra nw (Nodo v p c) = [Nodo (nw, b) (Nodo v p c) [] | (TP (b, _)) <- bestComb]
  where bestComb = take 10 (reverse (sort [TP (b, scoreTip b) | b <- allCombs, combCompat (nw, b) v (Nodo v p c)]))
crearNodosPalabra _ _ = []

--Raiz o papa, lista de palabras permitidas
crearNivelArbol :: Arbol Adivinacion -> [String] -> [Arbol Adivinacion]
crearNivelArbol _ [] = []
crearNivelArbol Hoja _ = []
crearNivelArbol (Nodo v p _) dic = concat hijosRes
  where
    palCompat = soloPalCompat dic v
    palPosibles = take 10 (sort (map (\w -> TP (w, scoreWord w)) palCompat))
    hijosRes = [crearNodosPalabra w (Nodo v p []) | (TP (w, _)) <- palPosibles]

crearArbol :: [String] -> Adivinacion -> Arbol Adivinacion
crearArbol pals ad = Nodo ad Hoja ([Nodo hv hp (crearNivelArbol (Nodo hv hp cs) pals) | (Nodo hv hp cs) <- hijos])
  where
    hijos = crearNivelArbol (Nodo ad Hoja []) pals

nodoMinimo :: Arbol Adivinacion -> Float
nodoMinimo (Nodo (a,b) _ []) = scoreWord a + scoreTip b
nodoMinimo (Nodo (a,b) _ cs) = scoreWord a + scoreTip b + minimum [nodoMinimo c | c <- cs]
nodoMinimo Hoja = 0.0

palabraFinal :: Arbol Adivinacion -> String
palabraFinal (Nodo _ _ cs) = fst minimo
  where minimo = minimumBy (\(_, b) (_, e) -> b `compare` e) (("TRAMPOSO", 99999999) : [(fst w, nodoMinimo (Nodo w p hc)) | (Nodo w p hc) <-cs])
palabraFinal Hoja = ""

damePalabra :: [String] -> Adivinacion -> String
damePalabra p a = palabraFinal (crearArbol p a)
