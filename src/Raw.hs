module Raw where
import Var

type RTy = Raw
data Raw
  = Var Name

  | Pi Name RTy RTy
  | Sg Name RTy RTy
  | Set  
  | Unit
  | Label
  | Enum

  | Lam Name Raw
  | App Raw  Raw

  | Pair Raw Raw
  | Fst Raw
  | Snd Raw

  | Bracket [Raw]

  | Tick String

  | Brace [Raw]

  | Let Name RTy Raw Raw
  deriving (Eq, Show)
