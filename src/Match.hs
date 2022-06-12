module Match where

data Match
  = Nada
  | Vaca
  | Toro
  deriving (Eq)

instance Show Match where
  show Toro = "T"
  show Vaca = "V"
  show Nada = "-"
