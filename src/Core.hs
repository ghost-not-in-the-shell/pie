module Core where
import Prelude hiding (pi)
import Var

newtype Bnd tm = Bnd tm
  deriving (Eq, Show)

type Ty = Tm
data Tm
  = FVar Name
  | BVar Ix

  | Pi Ty (Bnd Ty)
  | Sg Ty (Bnd Ty)
  | Set
  | Unit
  | Label
  | Enum
  | Tag Tm

  | Lam (Bnd Tm)
  | App Tm Tm

  | Pair Tm Tm
  | Fst Tm
  | Snd Tm

  | Sole

  | Tick String

  | Nil
  | Cons Tm Tm

  | Ze
  | Su Tm

  | Let Tm (Bnd Tm)
  deriving (Eq, Show)

close ∷ Name → Tm → Bnd Tm
close name = Bnd . go 0
  where bnd ∷ Int → Bnd Tm → Bnd Tm
        bnd acc (Bnd t) = Bnd (go (1 + acc) t)

        go ∷ Int → Tm → Tm
        go acc = \case
          FVar x | x == name → BVar (Ix acc)
                 | otherwise → FVar x
          BVar x   → BVar x
          Set      → Set
          Pi 𝕒 𝕓   → Pi (go acc 𝕒) (bnd acc 𝕓)
          Sg 𝕒 𝕓   → Sg (go acc 𝕒) (bnd acc 𝕓)
          Unit     → Unit
          Label    → Label
          Enum     → Enum
          Tag  t   → Tag  (go  acc t)
          Lam  t   → Lam  (bnd acc t)
          App  t u → App  (go  acc t) (go acc u)
          Pair t u → Pair (go  acc t) (go acc u)
          Fst  t   → Fst  (go  acc t)
          Snd  t   → Snd  (go  acc t)
          Sole     → Sole
          Tick l   → Tick l
          Nil      → Nil
          Cons t u → Cons (go  acc t) (go  acc u)
          Ze       → Ze
          Su   t   → Su   (go  acc t)
          Let  t u → Let  (go  acc t) (bnd acc u)

pi ∷ Name → Ty → Ty → Ty
pi x 𝕒 𝕓 = Pi 𝕒 (close x 𝕓)

arrow ∷ Ty → Ty → Ty
arrow 𝕒 𝕓 = pi "_" 𝕒 𝕓

sg ∷ Name → Ty → Ty → Ty
sg x 𝕒 𝕓 = Sg 𝕒 (close x 𝕓)

prod ∷ Ty → Ty → Ty
prod 𝕒 𝕓 = sg "_" 𝕒 𝕓

lam ∷ Name → Tm → Tm
lam x t = Lam (close x t)

let_ ∷ Name → Tm → Tm → Tm
let_ x body t = Let body (close x t)
