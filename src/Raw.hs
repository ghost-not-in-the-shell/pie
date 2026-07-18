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
  | Tag Raw
  | Case Raw Raw

  | Lam Name Raw
  | App Raw  Raw

  | Pair Raw Raw
  | Fst Raw
  | Snd Raw

  | Bracket [Raw]

  | Tick String

  | Brace [Raw]

  | Ze
  | Su Raw
  | Sharp String
  | Switch [(String , Raw)]

  | Let Name RTy Raw Raw
  deriving (Eq, Show)
