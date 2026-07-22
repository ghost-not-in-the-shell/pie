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
  | Desc
  | Nat
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
  | ElimEnum Tm Tm Tm Tm
  | Case Tm Tm

  | Ze Tm Tm
  | Su Tm Tm Tm
  | ElimTag Tm Tm Tm Tm
  | Switch Tm Tm Tm

  | Zero
  | Suc Tm

  | End
  | Arg Tm Tm
  | Rec Tm
  | El Tm Tm

  | Mu Tm
  | Inj Tm

  | Hyps Tm Tm Tm Tm
  | All Tm Tm Tm Tm Tm
  | Elim Tm Tm Tm

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
          Desc     → Desc
          Nat      → Nat
          Tag  t   → Tag  (go  acc t)
          Lam  t   → Lam  (bnd acc t)
          App  t u → App  (go  acc t) (go acc u)
          Pair t u → Pair (go  acc t) (go acc u)
          Fst  t   → Fst  (go  acc t)
          Snd  t   → Snd  (go  acc t)
          Zero     → Zero
          Suc  t   → Suc  (go  acc t)
          Sole     → Sole
          Tick l   → Tick l
          Nil      → Nil
          Cons t u → Cons (go  acc t) (go  acc u)
          End      → End
          Arg  t u → Arg  (go  acc t) (go  acc u)
          Rec  t   → Rec  (go  acc t)
          Case e p → Case (go  acc e) (go  acc p)
          Let  t u → Let  (go  acc t) (bnd acc u)
          Ze l e   → Ze (go acc l) (go acc e)
          Su l e t → Su (go acc l) (go acc e) (go acc t)
          ElimEnum scrut mot nil cons →
            ElimEnum (go acc scrut)
                     (go acc mot)
                     (go acc nil)
                     (go acc cons)

          ElimTag scrut mot ze su →
            ElimTag (go acc scrut)
                    (go acc mot)
                    (go acc ze)
                    (go acc su)

          Switch t p cs →
            Switch (go acc t)
                   (go acc p)
                   (go acc cs)

          El d x →
            El (go acc d) (go acc x)

          All d x p ϕ xs →
            All (go acc d) (go acc x) (go acc p) (go acc ϕ) (go acc xs)

          Mu d → Mu (go acc d)

          Inj ϕ → Inj (go acc ϕ)

          Hyps d x p ds → Hyps (go acc d) (go acc x) (go acc p) (go acc ds)
          Elim scrut p ϕ → Elim (go acc scrut) (go acc p) (go acc ϕ)

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

nat ∷ Integer → Tm
nat n | n == 0    = Zero
      | otherwise = Suc (nat (n-1))

{-
case_ ∷ Tm → Tm → Tm
case_ e p = ElimEnum e
  (lam "e" $ (Tag (FVar "e") `arrow` Set) `App` Set)
  (lam "P" $ Unit)
  (lam "l" $ lam "e" $ lam "ind" $ lam "P"
   (FVar "P" `App` (Ze (FVar "l") (FVar "e"))) `prod`
   (FVar "ind" `App` (lam "t" $ FVar "P" `App` Su (FVar "l") (FVar "e") (FVar "t"))))
  `App` p
-}
