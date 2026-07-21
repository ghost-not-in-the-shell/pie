module Val where
import Prelude hiding (pi)
import Var
import Core (Tm)

type Env = [Val]

data Clo val
  = HOAS  (val → val)
  | Subst (Tm  , Env)

instance Show (Clo val) where
  show (HOAS _) = "HOAS"
  show (Subst (t , ρ)) = show (t , ρ)

type VTy = Val
data Val
  = Pi VTy (Clo VTy)
  | Sg VTy (Clo VTy)
  | Set
  | Unit
  | Label
  | Enum
  | Desc
  | Nat
  | Tag Val
  
  | Lam (Clo Val)

  | Pair Val Val

  | Sole

  | Tick String

  | Nil
  | Cons Val Val

  | Ze Val Val
  | Su Val Val Val

  | Zero
  | Suc Val

  | End
  | Arg Val Val
  | Rec Val

  | Stuck (Ne , VTy)
  deriving (Show)

data Ne
  = Var Lv
  | App Ne (Val , VTy)
  | Fst Ne
  | Snd Ne
  | ElimEnum Ne Val Val Val
  | ElimTag  Ne Val Val Val
  | Case     Ne Val
  | Switch   Val Ne Val Val
  | El       Ne Val
  deriving (Show)

pi ∷ Val → (Val → Val) → Val
pi 𝕒 𝕓 = Pi 𝕒 (HOAS 𝕓)

arrow ∷ Val → Val → Val
arrow 𝕒 𝕓 = pi 𝕒 (const 𝕓)

sg ∷ Val → (Val → Val) → Val
sg 𝕒 𝕓 = Sg 𝕒 (HOAS 𝕓)

prod ∷ Val → Val → Val
prod 𝕒 𝕓 = sg 𝕒 (const 𝕓)

lam ∷ (Val → Val) → Val
lam body = Lam (HOAS body)
