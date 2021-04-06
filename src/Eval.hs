module Eval where
import Prelude hiding (lookup)
import Control.Monad.Reader
import Data.IntMap hiding (lookup)
import Impossible
import Debruijn
import MetaContext
import Tm  as T
import Val as V

data CmpR = CmpR { mcon ∷ IntMap MetaEntry            }
data EvR  = EvR  { mcon ∷ IntMap MetaEntry, env ∷ Env }

type Cmp = Reader CmpR
type Ev  = Reader EvR

lookup ∷ Ix → Ev Val
lookup (Ix x) = do
  ρ ← asks env
  return $ ρ !! x

lookupMeta ∷ Id → Cmp MetaEntry
lookupMeta (Id α) = do
  CmpR { mcon = mcon } ← ask
  return $ mcon ! α

bind ∷ Val → Ev a → Ev a
bind t = local (\ con → con { env = t : env con })

runCmp ∷ Cmp a → CmpR → a
runCmp = runReader

runEv ∷ Ev a → EvR → a
runEv = runReader

liftCmp ∷ Cmp a → Ev a
liftCmp m = do
  EvR { mcon = mcon } ← ask
  return $ runCmp m (CmpR { mcon = mcon })

liftEv ∷ Env → Ev a → Cmp a
liftEv env m = do
  CmpR { mcon = mcon } ← ask
  return $ runEv m (EvR { mcon = mcon, env = env })

stop ∷ Tm → Ev Clo
stop t = do
  ρ ← env <$> ask
  return $ (ρ,t)

resume ∷ Clo → Val → Cmp Val
resume (ρ,t) u = liftEv (u:ρ) $ eval t

app ∷ Val → Val → Cmp Val
app t u = case t of
  V.Lam t → resume t u
  V.Ne hd sp → return $ V.Ne hd (V.App u : sp)
  _ → impossible

appSp ∷ Val → Sp → Cmp Val
appSp t [] = return t
appSp t (V.App u : sp) = appSp t sp >>= (`app` u)
appSp t (V.Car   : sp) = appSp t sp >>= car
appSp t (V.Cdr   : sp) = appSp t sp >>= cdr

car ∷ Val → Cmp Val
car = \case
  V.Cons t _ → return t
  V.Ne hd sp → return $ V.Ne hd (V.Car : sp)
  _ → impossible

cdr ∷ Val → Cmp Val
cdr = \case
  V.Cons _ u → return u
  V.Ne hd sp → return $ V.Ne hd (V.Cdr : sp)
  _ → impossible

eval ∷ Tm → Ev Val
eval = \case
  T.Var x → lookup x

  T.Meta α → liftCmp (lookupMeta α) >>= \case
    Unsolved → return $ meta α
    Solved t → return $ t

  T.Set → return V.Set

  T.Pi _A _B → do
    _A ← eval _A
    _B ← stop _B
    return $ V.Pi _A _B

  T.Lam t → do
    t ← stop t
    return $ V.Lam t

  T.App t u → do
    t ← eval t
    u ← eval u
    liftCmp $ app t u

  T.Sg _A _B → do
    _A ← eval _A
    _B ← stop _B
    return $ V.Sg _A _B

  T.Cons t u → do
    t ← eval t
    u ← eval u
    return $ V.Cons t u

  T.Car t → do
    t ← eval t
    liftCmp $ car t

  T.Cdr t → do
    t ← eval t
    liftCmp $ cdr t

  T.Let t u → do
    t ← eval t
    bind t $ eval u

force ∷ Val → Cmp Val
force t = case t of
  V.Flex α sp → lookupMeta α >>= \case
    Solved t → appSp t sp
    Unsolved → return $ t
  _ → return t
