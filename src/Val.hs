module Val where
import Var

type Env = [Val]

type Clo val = val → val

type VTy = Val
data Val
  = Pi VTy (Clo VTy)
  | Sg VTy (Clo VTy)
  | Set
  | Unit
  | Label
  | Enum
  | Tag Val
  
  | Lam (Clo Val)

  | Pair Val Val

  | Sole

  | Tick String

  | Nil
  | Cons Val Val

  | Ze
  | Su Val

  | Stuck (Ne , VTy)

data Ne
  = Var Lv
  | App Ne (Val , VTy)
  | Fst Ne
  | Snd Ne
