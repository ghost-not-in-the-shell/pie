module Syntax.Constraint where
import Syntax.NormalForm

type Heterogeneous = (Con, ((Nf, Nf), (Nf, Nf)))

type Homogeneous = (Con, ((Nf, Nf), Nf))
data Guarded where
  (:>>:) ∷ [Homogeneous] → Homogeneous → Guarded

infix 4 ⊢
a ⊢ b = (a, b)

infix 5 ≅
a ≅ b = (a, b)

infix 6 ∶
a ∶ b = (a, b)

infix 7 ≡
a ≡ b = (a, b)
