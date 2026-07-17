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

  | Lam (Bnd Tm)
  | App Tm Tm

  | Pair Tm Tm
  | Fst Tm
  | Snd Tm

  | Sole

  | Tick String

  | Nil
  | Cons Tm Tm

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
          Lam  t   → Lam  (bnd acc t)
          App  t u → App  (go  acc t) (go acc u)
          Pair t u → Pair (go  acc t) (go acc u)
          Fst  t   → Fst  (go  acc t)
          Snd  t   → Snd  (go  acc t)
          Sole     → Sole
          Tick l   → Tick l
          Nil      → Nil
          Cons t u → Cons (go  acc t) (go  acc u)
          Let  t u → Let  (go  acc t) (bnd acc u)

pi ∷ Name → Tm → Tm → Tm
pi x 𝕒 𝕓 = Pi 𝕒 (close x 𝕓)

arrow ∷ Tm → Tm → Tm
arrow 𝕒 𝕓 = pi "_" 𝕒 𝕓

sg ∷ Name → Tm → Tm → Tm
sg x 𝕒 𝕓 = Sg 𝕒 (close x 𝕓)

prod ∷ Tm → Tm → Tm
prod 𝕒 𝕓 = sg "_" 𝕒 𝕓

lam ∷ Name → Tm → Tm
lam x t = Lam (close x t)

let_ ∷ Name → Tm → Tm → Tm
let_ x body t = Let body (close x t)
