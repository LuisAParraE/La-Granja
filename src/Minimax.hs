
module Minimax (damePalabra) where

import Match
import Data.List

-- | Representa una pista del juego
type Combinacion = [Match]
type Adivinacion = (String, Combinacion)

-- | Tupla que contiene un valor y una puntuacion
newtype TuplaPuntuada v =
    TP (v, Float)
    deriving (Show)

instance Eq (TuplaPuntuada v) where
  (TP (_, s)) == (TP (_, s2)) = s == s2

instance Ord (TuplaPuntuada v) where
  (TP (_, s)) `compare` (TP (_, s2)) = s `compare` s2


-- | Tipo que representa un arbol
data Arbol v =
      Hoja
    | Nodo v (Arbol v) [Arbol v]

  deriving (Show)

-- | Calcula el valor de una letra
letterValue :: Char   -- ^ Caracter a calcular valor
            -> Float  -- ^ Valor caracter
letterValue x
  | x == 'A' || x == 'E' = 0.1
  | x == 'I' || x == 'N' || x == 'O' || x == 'R' || x == 'S' = 0.2
  | x == 'D' || x == 'L' || x == 'C' || x == 'T' || x == 'U' = 0.3
  | x == 'B' || x == 'G' || x == 'M' || x == 'P' = 0.5
  | x == 'F' || x == 'H' || x == 'Q' || x == 'V' || x == 'Y' = 0.8
  | x == 'J' || x == 'K' || x == 'W' || x == 'X' || x == 'Z' = 1
  | otherwise = 0

-- | Valores de Vaca, Toro y Nada
matchValue :: Fractional p => Match   -- ^ Valor a convertir
                      -> p            -- ^ Valor retorno
matchValue Vaca = 0.1
matchValue Toro = 0.2
matchValue _ = 0

-- | Calcula el valor de una palabra
scoreWord :: String   -- ^ Palabra a calcular valor
          -> Float    -- ^ Valor de la palabra
scoreWord [] = 0
scoreWord xs = sum $ map letterValue xs

-- | Calcula el valor de una pista
scoreTip :: Fractional p => Combinacion -- ^ Pista
                          -> p          -- ^ Valor pista
scoreTip x = 1.0 - sum (map matchValue x)

{-| 
  Filtra las palabras que sean compatibles con una
  adivinacion.
-}
soloPalCompat :: [String]         -- ^ Palabras ya usadas
              -> [String]         -- ^ Palabras a testear
              -> Adivinacion      -- ^ Adivinacion a usar
              -> [String]         -- ^ Palabras compatibles
soloPalCompat ant x y = filter (\w -> palCompat ant w y) x

-- | Cuenta un caracter en un string
countChar :: Char       -- ^ Caracter a contar
          -> String     -- ^ Palabra a buscar caracter
          -> Int        -- ^ Numero de veces que aparece el caracter
countChar x y = sum [1 | c <- y, c == x]

{-|
  Cuenta cuantas veces en una adivinacion, un caracter es
  una Vaca o un Toro.
-}
countCharVacasToros :: Char           -- ^ Caracter a buscar
                    -> Adivinacion    -- ^ Adivinacion a examinar
                    -> Int            -- ^ Veces que el caracter es vaca o toro
countCharVacasToros x (y, z) = sum [1 | (c, v) <- zip y z, (v == Vaca || v == Toro) && c == x]

{-|
  Revisa si una palabra tiene los mismos caracteres en posiciones
  toros de una adivinacion (deberia usarse con un adiv. anterior)
-}
torosBienPal :: String          -- ^ Palabra a probar
              -> Adivinacion    -- ^ Adivinacion
              -> Bool           -- ^ True si los toros tienen los mismos caracteres
torosBienPal x (y, z) = all (\(g,h,i) -> (i /= Toro) || (g == h)) (zip3 x y z)

{-|
  Revisa si una palabra no tiene los mismos caracteres en posiciones
  vacas de una adivinacion (deberia usarse con una adiv. anterior)
-}
noVacasIgual :: String          -- ^ Palabra a probar
              -> Adivinacion    -- ^ Adivinacion
              -> Bool           -- ^ True si las vacas tienen distintos caracteres
noVacasIgual x (y, z) = all (\(g,h,i) -> (i /= Vaca) || (g /= h)) (zip3 x y z)

{-|
  Revisa si para todos los caracteres vaca de una adivinacion, hay mas
  o igual cantidad de este caracter en una palabra (deberia usarse con
  una adiv. anterior)
-}
vacasMantenidas :: String         -- ^ Palabra a probar
                -> Adivinacion    -- ^ Adivinacion
                -> Bool           -- ^ True si se cumple para todas las vacas
vacasMantenidas x (y, z) = all (\(h,i) -> (i /= Vaca) || (countChar h y <= countChar h x)) (zip y z)

{-|
  Revisa si una palabra es compatible con las vacas de una
  adivinacion (deberia usarse con una adiv. anterior)
-}
vacasBienPal :: String          -- ^ Palabra a probar
              -> Adivinacion    -- ^ Adivinacon
              -> Bool           -- True la palabra es compatible con las vacas de la adiv.
vacasBienPal x y = noVacasIgual x y && vacasMantenidas x y

{-|
  Se asegura que no haya ningun caracter extra en una palabra
  respecto a una adivinacion 
-}
ningBienPal :: String         -- ^ Palabra a probar
            -> Adivinacion    -- ^ Adivinacion anterior
            -> Bool           -- ^ True si la palabra es compatible con la adivinacion. False en caso contrario
ningBienPal x (y, z) = all (\(h,i) -> (i /= Nada) || (countCharVacasToros h (y, z) == countChar h x)) (zip y z)

{-|
  Revisa si una palabra es compatible con una adivinacion (deberia usarse 
  con una adiv. anterior)
-}
palCompat :: [String]
            -> String         -- ^ Palabra a probar
            -> Adivinacion    -- ^ Adivinacion
            -> Bool           -- ^ True si la palabra es compatible con la adiv.
palCompat ant x (w, t) =  notElem x ant && x /= w && torosBienPal x (w, t) && vacasBienPal x (w, t) && ningBienPal x (w, t)

-- | Lista con todas las combinaciones de tamano 5 de Vacas, Toros y Nadas.
allCombs :: [Combinacion]
allCombs = [[x, y, z, w, k] | x <- m, y <- m, z <- m, w <- m, k <- m]
    where m = [Vaca, Toro, Nada]

-- | Revisa que dos combinaciones tengan los mismos toros
mismosToros :: Combinacion    -- ^ Combinacion 1
            -> Combinacion    -- ^ Combinacion 2
            -> Bool           -- ^ True si se cumple
mismosToros x t = all (\(a, b) -> (b /= Toro) || (a == b)) (zip x t)

{-|
  Busca una vaca o toro que tenga un caracter en especifico en una
  adivinacion, reemplaza por un Nada este caracter y devuelve la
  nueva combinacion
-}
buscaQuitaVT :: Char          -- ^ Caracter a buscar
            -> Adivinacion    -- ^ Adivinacion a cambiar combinacion
            -> Combinacion    -- ^ Nueva combinacion
buscaQuitaVT c (w:ws, x:xs)
  | w /= c = x : buscaQuitaVT c (ws, xs)
  | otherwise = if x /= Nada then Nada : xs else x : buscaQuitaVT c (ws, xs)

buscaQuitaVT _ _ = []

{-|
  Revisa si un caracter tiene una vaca o toro en una adivinacion,
  lo reemplaza por un Nada si hay, y devuelve una tuple que indica
  si se encontro el valor y la nueva adivinacion.
-}
tieneVacaToro :: Char                 -- ^ Caracter a buscar
              -> Adivinacion          -- ^ Adivinacion a modificar
              -> (Bool, Adivinacion)  -- ^ (Se encontro o no valor, Nueva adivinacion)
tieneVacaToro c (w, t) = (com /= t, (w, com))
  where com = buscaQuitaVT c (w, t)

{-|
  Revisa que las vacas de una posible adivinacion, sean compatibles 
  con una adivinacion anterior
-}
vacasBienCom :: Adivinacion     -- ^ Nueva adivinacion a probar
              -> Adivinacion    -- ^ Vieja adivinacion
              -> Bool           -- ^ True si la nueva adivinacion es compatible con la vieja
vacasBienCom (nw, nt) (w:ws, t:ts)
  | t /= Vaca = vacasBienCom (nw, nt) (ws, ts)
  | otherwise = res && vacasBienCom adiv (ws, ts)
    where (res, adiv) = tieneVacaToro w (nw, nt)

vacasBienCom _ _ = True

{-|
  Revisa si ya se probo una combinacion en alguno de los nodos de una rama
  del arbol minimax empezando en un nodo especifico
-}
combAntes :: Arbol Adivinacion    -- ^ Arbol a buscar
          -> Combinacion          -- ^ Combinaicion a buscar
          -> Bool                 -- ^ True si la combinacion se encuentra en un padre
combAntes (Nodo (w, v) p _) c
  | v == c = True
  | otherwise = combAntes p c
combAntes Hoja _ = False

-- | Revisa que una adivinacion sea compatible con otra adivinacion
combCompat :: Adivinacion         -- ^ Nueva adivinacion
            -> Adivinacion        -- ^ Vieja adivinacion
            -> Arbol Adivinacion  -- ^ Arbol padre de la nueva adivinacion
            -> Bool               -- ^ True si la nueva adivinacion es compatible con la vieja
combCompat (x, y) (z, w) a = mismosToros y w && vacasBienCom (x, y) (z, w) && not (combAntes a y)

{-|
  Crea los nodos hijos de una palabra. Prueba todas las combinaciones
  posibles y revisa compatibilidad con ellos. Asume que la palabra ya
  es compatible.
-}
crearNodosPalabra :: String               -- ^ Palabra a crear nodos
                  -> Arbol Adivinacion    -- ^ Arbol asociado a la palabra
                  -> [Arbol Adivinacion]  -- ^ Hijos resultantes del nodo
crearNodosPalabra nw (Nodo v p c) = [Nodo (nw, b) (Nodo v p c) [] | (TP (b, _)) <- bestComb]
  where bestComb = take 10 (reverse (sort [TP (b, scoreTip b) | b <- allCombs, combCompat (nw, b) v (Nodo v p c)]))
crearNodosPalabra _ _ = []

{-|
  Crea un subnivel del arbol
-}
crearNivelArbol :: [String]               -- ^ Palabras ya usadas en rondas anteriores
                  -> Arbol Adivinacion    -- ^ Raiz del arbol
                  -> [String]             -- ^ Palabras posibles a usar
                  -> [Arbol Adivinacion]  -- ^ Hijos de la raiz
crearNivelArbol _ _ [] = []
crearNivelArbol _ Hoja _ = []
crearNivelArbol ant (Nodo v p _) dic = concat hijosRes
  where
    palCompat = soloPalCompat ant dic v
    palPosibles = take 10 (sort (map (\w -> TP (w, scoreWord w)) palCompat))
    hijosRes = [crearNodosPalabra w (Nodo v p []) | (TP (w, _)) <- palPosibles]

{-|
  Crea el arbol minimax
-}
crearArbol :: [String]            -- ^ Palabras ya usadas en rondas anteriores
            -> [String]           -- ^ Palabras posibles a usar
            -> Adivinacion        -- ^ Adivinacion anterior
            -> Arbol Adivinacion  -- ^ Arbol minimax
crearArbol ant pals ad = Nodo ad Hoja ([Nodo hv hp (crearNivelArbol ant (Nodo hv hp cs) pals) | (Nodo hv hp cs) <- hijos])
  where
    hijos = crearNivelArbol ant (Nodo ad Hoja []) pals

{-|
  Calcula el valor de la rama con menor valor
-}
nodoMinimo :: Arbol Adivinacion   -- ^ Raiz del arbol a examinar
            -> Float              -- ^ Valor de la rama co menor valor
nodoMinimo (Nodo (a,b) _ []) = scoreWord a + scoreTip b
nodoMinimo (Nodo (a,b) _ cs) = scoreWord a + scoreTip b + minimum [nodoMinimo c | c <- cs]
nodoMinimo Hoja = 0.0

-- | Busca la palabra con menor valor del arbol minimax generado
palabraFinal :: Arbol Adivinacion   -- ^ Raiz arbol minimax
              -> String             -- ^ Palabra elegida
palabraFinal (Nodo _ _ cs) = fst minimo
  where minimo = minimumBy (\(_, b) (_, e) -> b `compare` e) (( "TRAMPOSO", 99999999) : [(fst w, nodoMinimo (Nodo w p hc)) | (Nodo w p hc) <- cs])
palabraFinal Hoja = ""

{-|
  Dado una lista de palabras y una adivinacion anterior,
  mediante el arbol minimax se elige una palabra para 
  adivinar de vuelta.
-}
damePalabra :: [String]       -- ^ Palabras ya usadas en rondas anteriores
              ->[String]      -- ^ Palabras posibles a usar
              -> Adivinacion  -- ^ Adivinacion anterior
              -> String       -- ^ Palabra final. Si la palabra es TRAMPOSO, el jugador hizo trampa.
damePalabra ant p a = palabraFinal (crearArbol ant p a)
