module Tm where
import Debruijn

data Tm
  = Var  Ix
  | Meta Id

  | Set

  | Pi  Tm Tm
  | Lam Tm
  | App Tm Tm

  | Sg   Tm Tm
  | Cons Tm Tm
  | Car  Tm
  | Cdr  Tm

  | Let Tm Tm
