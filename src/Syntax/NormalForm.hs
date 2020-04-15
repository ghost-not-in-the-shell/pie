module Syntax.NormalForm where
import GHC.Generics
import Prelude hiding (pi)
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

data Nf where
  Set   ∷ Nf
  Void  ∷ Nf
  Unit  ∷ Nf
  Sum   ∷ Nf → Nf → Nf
  Pi    ∷ Bind (Name Nf, Embed Nf) Nf → Nf
  Sigma ∷ Bind (Name Nf, Embed Nf) Nf → Nf

  Sole ∷ Nf
  Cons ∷ Nf → Nf → Nf
  Inj₁ ∷ Nf → Nf
  Inj₂ ∷ Nf → Nf
  Lam  ∷ Bind (Name Nf) Nf → Nf
  Ne   ∷ Hd → [Elim] → Nf
  deriving (Generic, Show)

data Hd where
  Var  ∷ Name Nf → Hd
  Meta ∷ Name Nf → Hd
  deriving (Generic, Show)

data Elim where
  App     ∷ Nf → Elim
  Car     ∷ Elim
  Cdr     ∷ Elim
  Case    ∷ Nf → Nf → Nf → Elim
  ExFalso ∷ Nf → Elim
  deriving (Generic, Show)

type Con = [(Name Nf, Nf)]
type Sig = [(Name Nf, Nf)]

instance Alpha Nf
instance Alpha Hd
instance Alpha Elim

instance Eq Nf where
  (==) = aeq

instance Eq Elim where
  (==) = aeq

set = Set
void = Void
unit = Unit
sum  = Sum

pi    ∷ Name Nf → Nf → Nf → Nf
sigma ∷ Name Nf → Nf → Nf → Nf
pi    x _S _T = Pi    (bind (x, embed _S) _T)
sigma x _S _T = Sigma (bind (x, embed _S) _T)

(⇨) ∷ Nf → Nf → Nf
_S ⇨ _T = pi (s2n "_") _S _T

(⟶) ∷ Con → Nf → Nf
[] ⟶ _T = _T
((x,_S):_Γ) ⟶ _T = _Γ ⟶ pi x _S _T

sole = Sole
cons = Cons
inj₁ = Inj₁
inj₂ = Inj₂

var  ∷ Name Nf → Nf
meta ∷ Name Nf → Nf
var  x = Ne (Var  x) []
meta m = Ne (Meta m) []

lam ∷ Name Nf → Nf → Nf
lam x t = Lam (bind x t)

-- Γ ⊢       t : Π(x:S).T
-- Γ ⊢       u : S
-- ----------------------
-- Γ ⊢ app t u : T[x≔S]
app ∷ Nf → Nf → Nf
app (Lam (unsafeUnbind → (x, t))) u = subst x u t
app (Ne h sp) u = Ne h (sp ++ [App u])

appⁿ ∷ Nf → [Nf] → Nf
appⁿ t [] = t
appⁿ t (u : us) = appⁿ (app t u) us

car ∷ Nf → Nf
car (Cons t₁ t₂) = t₁
car (Ne h sp) = Ne h (sp ++ [Car])

cdr ∷ Nf → Nf
cdr (Cons t₁ t₂) = t₂
cdr (Ne h sp) = Ne h (sp ++ [Cdr])

case' ∷ Nf → Nf → Nf → Nf → Nf
case' (Inj₁ t) _P u₁ u₂ = app u₁ t
case' (Inj₂ t) _P u₁ u₂ = app u₂ t
case' (Ne h sp) _P u₁ u₂ = Ne h (sp ++ [Case _P u₁ u₂])

exFalso ∷ Nf → Nf → Nf
exFalso (Ne h sp) _P = Ne h (sp ++ [ExFalso _P])

elim ∷ Nf → Elim → Nf
elim t (App u) = app t u
elim t (Car  ) = car t
elim t (Cdr  ) = cdr t
elim t (Case    _P u₁ u₂) = case'   t _P u₁ u₂
elim t (ExFalso _P      ) = exFalso t _P

type Sub = [(Name Nf, Nf)]

instance Subst Nf Nf where
  substs ∷ Sub → Nf → Nf
  substs ρ (Set      ) = Set
  substs ρ (Void     ) = Void
  substs ρ (Unit     ) = Unit
  substs ρ (Sum _S _T) = Sum   (substs ρ _S) (substs ρ _T)
  substs ρ (Pi      b) = Pi    (substs ρ  b)
  substs ρ (Sigma   b) = Sigma (substs ρ  b)
  substs ρ (Sole     ) = Sole
  substs ρ (Inj₁    t) = Inj₁  (substs ρ  t)
  substs ρ (Inj₂    t) = Inj₂  (substs ρ  t)
  substs ρ (Lam     b) = Lam   (substs ρ  b)
  substs ρ (Ne   h sp) = foldl elim (substsHd ρ h) (substs ρ sp)

  subst ∷ Name Nf → Nf → Nf → Nf
  subst x u t = substs [(x,u)] t

substsHd ∷ Sub → Hd → Nf
substsHd ρ (Var  x) | Just t ← lookup x ρ = t
substsHd ρ (Meta m) | Just t ← lookup m ρ = t
substsHd ρ h = Ne h []

instance Subst Nf Elim
