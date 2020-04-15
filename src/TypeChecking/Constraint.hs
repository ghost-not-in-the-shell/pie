module TypeChecking.Constraint where
import Syntax.NormalForm

type Constraint = (Con, ((Nf, Nf), (Nf, Nf)))

infix 4 ⊢
a ⊢ b = (a, b)

infix 5 ≅
a ≅ b = (a, b)

infix 6 ∶
a ∶ b = (a, b)

