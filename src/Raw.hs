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
  | Desc
  | Nat
  | Tag Raw

  | Lam Name (Maybe RTy) Raw
  | App Raw  Raw

  | Pair Raw Raw
  | Fst Raw
  | Snd Raw

  | Bracket [Raw]

  | Tick String

  | Nil
  | Cons Raw Raw
  | Brace [Raw]
  | ElimEnum Raw Raw Raw Raw
  | Case Raw Raw

  | Ze
  | Su Raw
  | Sharp String
  | ElimTag Raw Raw Raw Raw
  | Switch Raw Raw [(String , Raw)]

  | Zero
  | Suc Raw

  | End
  | Arg Raw Raw
  | Rec Raw

  | Let Name RTy Raw Raw
  deriving (Eq, Show)

lam ∷ Name → Raw → Raw
lam x body = Lam x Nothing body

nat ∷ Integer → Raw
nat n | n == 0    = Zero
      | otherwise = Suc (nat (n-1))
