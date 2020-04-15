module Example where
import Syntax.NormalForm hiding (Set, Void, Unit, Sum, Pi, Sigma, Sole, Cons, Inj₁, Inj₂, Lam, Var, App, Car, Cdr, Case, ExFalso)
import Syntax.Pretty
import Syntax.Term
import TypeChecking.Elaboration

ty1 = unit ⇨ unit
tm1 = Lam "x" (Var "x")

result = runElab $ elab tm1 ty1

{-
[
0~ x : ?0 ⊢ x              : ?0              ≅ (?2 x) : (?1 x)
1~        ⊢ (λ (x) (?2 x)) : ∀ (x ?0) (?1 x) ≅ ?3     : ∀ (_ ⊤) ⊤
]

⇒

[
0~   ...
1.0+        ⊢ ∀ (x ?0) (?1 x) ≡ ∀ (_ ⊤) ⊤ : Set
1.1+ y : ?0 ⊢ ((λ (x) (?2 x)) y) ≡ ?3 y : ∀ (x ?0) (?1 x)
]

⇒

[
0~ ...
1.0.1+        ⊢ ?0 ≡ ⊤ : Set
1.0.2+ x : ?0 ⊢ ?1 x ≡ ⊤ : Set
1.1+ ...
]

⇒

[
0~ x : ⊤ ⊢ x : ⊤ ≅ 
]
-}
