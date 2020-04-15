module TypeChecking.Unification where
import Syntax.Constraint
import Syntax.NormalForm
import Unbound.Generics.LocallyNameless

simplify ∷ Fresh m ⇒ [Homogeneous] → m ()
simplify = undefined

unify ∷ Fresh m ⇒ Homogeneous → m ()
unify (_Γ, ((t₁, t₂), Pi b)) = do
  ((x, unembed → _S), _T) ← unbind b
  simplify [ (x,_S):_Γ ⊢ app t₁ (var x) ≡ app t₂ (var x) ∶ _T ]
unify (_Γ, ((t₁, t₂), Sigma b)) = do
  ((x, unembed → _S), _T) ← unbind b
  simplify [ _Γ ⊢ car t₁ ≡ car t₂ ∶ _S
           , _Γ ⊢ cdr t₁ ≡ cdr t₂ ∶ subst x (car t₁) _T ]
