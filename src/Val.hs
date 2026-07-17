module Val where
import Var
import Core

newtype Clo tm = Clo (tm , Env)
  deriving (Eq, Show)

type Env = [Val]

type VTy = Val
data Val
  = Pi VTy (Clo Ty)
  | Sg VTy (Clo Ty)
  | Set
  | Unit
  | Label
  | Enum
  
  | Lam (Clo Tm)

  | Pair Val Val

  | Sole

  | Tick String

  | Nil
  | Cons Val Val

  | Stuck (Ne , VTy)
  deriving (Eq, Show)

data Ne
  = Var Lv
  | App Ne (Val , VTy)
  | Fst Ne
  | Snd Ne
  deriving (Eq, Show)
