module Val where
import Debruijn
import Tm (Tm)

data Val
  = Set

  | Pi  Val Clo
  | Lam Clo

  | Sg   Val Clo
  | Cons Val Val

  | Ne Hd Sp

type Clo = (Env, Tm)

type Env = [Val]

data Hd = Var  Lv
        | Meta Id

data Frm
  = App Val
  | Car
  | Cdr

type Sp = [Frm]

pattern Rigid ∷ Lv → Sp → Val
pattern Flex  ∷ Id → Sp → Val
pattern Rigid x sp = Ne (Var  x) sp
pattern Flex  α sp = Ne (Meta α) sp

var  ∷ Lv → Val
meta ∷ Id → Val
var  x = Ne (Var  x) []
meta α = Ne (Meta α) []
