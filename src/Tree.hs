module Tree where

data Tree v p
  = Leaf
  | Node
      { value :: v,
        points :: p,
        lTree :: Tree v p,
        rtree :: Tree v p
      }
  deriving (Show, Eq, Read, Ord)

letterValue x
  | x == 'a' || x == 'e' = 0.1
  | x == 'i' || x == 'n' || x == 'o' || x == 'r' || x == 's' = 0.2
  | x == 'd' || x == 'l' || x == 'c' || x == 't' || x == 'u' = 0.3
  | x == 'b' || x == 'g' || x == 'm' || x == 'p' = 0.5
  | x == 'f' || x == 'h' || x == 'q' || x == 'v' || x == 'y' = 0.8
  | x == 'j' || x == 'k' || x == 'w' || x == 'x' || x == 'z' = 1
  | otherwise = 0

score [] = 0
score xs = sum $ map letterValue xs