module Raw where

data Raw
  = Var String
  | Hole

  | Set

  | Pi  String Raw Raw
  | Lam String Raw
  | App Raw  Raw

  | Sg   String Raw Raw
  | Cons Raw  Raw
  | Car  Raw
  | Cdr  Raw

  | Let String Raw Raw Raw
  deriving (Eq, Show)
