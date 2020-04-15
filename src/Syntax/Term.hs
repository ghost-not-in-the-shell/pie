module Syntax.Term where

data Tm where
  Set   ∷ Tm
  Void  ∷ Tm
  Unit  ∷ Tm
  Sum   ∷ Tm → Tm → Tm
  Pi    ∷ String → Tm → Tm → Tm
  Sigma ∷ String → Tm → Tm → Tm

  Sole    ∷ Tm
  Cons    ∷ Tm → Tm → Tm
  Car     ∷ Tm → Tm
  Cdr     ∷ Tm → Tm
  Inj₁    ∷ Tm → Tm
  Inj₂    ∷ Tm → Tm
  Case    ∷ Tm → Tm → Tm → Tm → Tm
  ExFalso ∷ Tm → Tm → Tm
  Var     ∷ String → Tm
  Lam     ∷ String → Tm → Tm
  App     ∷ Tm → Tm → Tm

  Hole ∷ Tm
