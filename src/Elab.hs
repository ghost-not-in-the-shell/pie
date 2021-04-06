module Elab where
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap hiding (size)
import Data.List
import Debruijn
import MetaContext
import Raw  as R
import Tm   as T
import Val  as V
import Eval hiding (lookup, lookupMeta, topVar, bind, liftCmp, liftEv, env)
import qualified Eval as Eval
import Unify hiding (topVar, liftCmp, liftEv, fresh, size, bind)

data ElabR = ElabR
  { iden ∷ [String]
  , bnds ∷ Env
  , tele ∷ Env
  , env  ∷ Env
  , size ∷ Int
  }

data ElabS = ElabS { mcon ∷ IntMap MetaEntry, fresh ∷ Int }

data ElabE = UnboundVariable | FailToUnify

type Elab = ReaderT ElabR
          ( StateT  ElabS
          ( Except  ElabE ))

lookup ∷ Ix → Elab Val
lookup x = liftEv $ Eval.lookup x

lookupMeta ∷ Id → Elab MetaEntry
lookupMeta α = liftCmp $ Eval.lookupMeta α

topVar ∷ Elab Val
topVar = do
  n ← asks size
  return $ var $ Lv $ n-1

bind ∷ String → Val → (Val → Elab a) → Elab a
bind x _A k = do
  n ← asks size
  let t = var $ Lv n
  local (\ con → con
          { iden =  x:iden con
          , bnds =  t:bnds con
          , tele = _A:tele con
          , env  =  t:env  con
          , size =  1+size con
          }) $ k =<< topVar

bindEval ∷ String → Tm → (Val → Elab a) → Elab a
bindEval x _A k = do
  _A ← liftEv (eval _A)
  bind x _A k

define ∷ String → Val → Val → Elab a → Elab a
define x _A t m = local (\ con → con
                          { iden =  x:iden con
                          , bnds =    bnds con
                          , tele = _A:tele con
                          , env  =  t:env  con
                          , size =  1+size con
                          }) m

freshMeta ∷ Elab Tm
freshMeta = do
  bnds ← asks bnds
  α    ← gets fresh
  return $ appⁿ (T.Meta (Id α)) undefined
  where appⁿ t [] = t
        appⁿ t (x:sp) = T.App (appⁿ t sp) x

liftCmp ∷ Cmp a → Elab a
liftCmp m = do
  ElabS { mcon = mcon } ← get
  return $ runCmp m (CmpR { mcon = mcon })

liftEv ∷ Ev a → Elab a
liftEv m = do
  ElabR { env  = env  } ← ask
  ElabS { mcon = mcon } ← get
  return $ runEv m (EvR { mcon = mcon, env = env })

liftUnif ∷ Unif a → Elab a
liftUnif = undefined

check ∷ Raw → Val → Elab Tm
check t _A = do
  (t, _B) ← infer t
  liftUnif $ unify _A _B
  return t

infer ∷ Raw → Elab (Tm, Val)
infer (R.Var x) = do
  xs ← asks iden
  _Γ ← asks tele
  case elemIndex x xs of
    Nothing → throwError UnboundVariable
    Just x  → return (T.Var (Ix x), _Γ !! x)

infer R.Hole = do
  t  ← freshMeta
  _A ← freshMeta >>= liftEv . eval
  return (t, _A)

infer R.Set = return (T.Set, V.Set)

infer (R.Pi x _A _B) = do
  _A ← check _A V.Set
  _B ← bindEval x _A $ \ _ → check _B V.Set
  return (T.Pi _A _B, V.Set)

infer (R.Lam x t) = do
  _A      ← freshMeta >>= liftEv . eval
  (t, _B) ← bind x _A $ \ _ → infer t
  return (T.Lam t, V.Pi _A $ undefined)

infer (R.App t u) = do
  (t, _S) ← infer t

  (_A, _B) ← liftCmp (force _S) >>= \case
    V.Pi _A _B →
      return (_A, _B)
    _S → do
      _A ← freshMeta >>= liftEv . eval
      _B ← bind "x" _A $ \ _ → freshMeta >>= liftEv . stop
      liftUnif $ unify _S (V.Pi _A _B)
      return (_A, _B)

  u ← check u _A
  _B ← liftEv (eval u) >>= (\ u → liftCmp (resume _B u))
  return (T.App t u, _B)

infer (R.Sg x _A _B) = do
  _A ← check _A V.Set
  _B ← bindEval x _A $ \ _ → check _B V.Set
  return (T.Sg _A _B, V.Set)

infer R.Hole = do
  _A ← freshMeta >>= liftEv . eval
  t  ← freshMeta
  return (t, _A)


