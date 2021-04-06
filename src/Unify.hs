module Unify where
import Prelude hiding (lookup, map)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.IntMap hiding (map, size)
import Debruijn
import MetaContext
import qualified Tm as T
import Tm          (Tm)
import Val
import Eval hiding (lookup, bind, liftCmp, liftEv)

data UnifR = UnifR { size ∷ Int                           }
data UnifS = UnifS { mcon ∷ IntMap MetaEntry, fresh ∷ Int }
data UnifE = UnifyError

type Unif = ReaderT UnifR
          ( StateT  UnifS
          ( Except  UnifE ))

topVar ∷ Unif Val
topVar = do
  n ← asks size
  return $ var $ Lv $ n-1

bind ∷ (Val → Unif a) → Unif a
bind k = local (\ con → con { size = 1 + size con }) $ k =<< topVar

(≔) ∷ Id → Val → Unif ()
Id α ≔ solut = modify $ \ UnifS { mcon = mcon, fresh = fresh } → UnifS
  { mcon  = insert α (Solved solut) mcon
  , fresh = fresh
  }

liftCmp ∷ Cmp a → Unif a
liftCmp m = do
  UnifS { mcon = mcon } ← get
  return $ runCmp m (CmpR { mcon = mcon })

liftEv ∷ Env → Ev a → Unif a
liftEv env m = do
  UnifS { mcon = mcon } ← get
  return $ runEv m (EvR { mcon = mcon, env = env })

unify ∷ Val → Val → Unif ()
unify t u = do
  t ← liftCmp $ force t
  u ← liftCmp $ force u
  case (t, u) of
    (Set, Set) → return ()

    (Pi _A₁ _B₁, Pi _A₂ _B₂) → do
      unify _A₁ _A₂
      bind $ \ x → do
        _B₁ ← liftCmp $ resume _B₁ x
        _B₂ ← liftCmp $ resume _B₂ x
        unify _B₁ _B₂

    (Lam t₁, Lam t₂) →
      bind $ \ x → do
        t₁ ← liftCmp $ resume t₁ x
        t₂ ← liftCmp $ resume t₂ x
        unify t₁ t₂

    (Lam t₁, t₂) →
      bind $ \ x → do
        t₁ ← liftCmp $ resume t₁ x
        t₂ ← liftCmp $ app    t₂ x
        unify t₁ t₂

    (t₁, Lam t₂) →
      bind $ \ x → do
        t₁ ← liftCmp $ app    t₁ x
        t₂ ← liftCmp $ resume t₂ x
        unify t₁ t₂

    (Sg _A₁ _B₁, Sg _A₂ _B₂) → do
      unify _A₁ _A₂
      bind $ \ x → do
        _B₁ ← liftCmp $ resume _B₁ x
        _B₂ ← liftCmp $ resume _B₂ x
        unify _B₁ _B₂

    (Cons t₁ u₁, Cons t₂ u₂) →
      unify t₁ t₂ >> unify u₁ u₂

    (Cons t₁ u₁, t₂u₂) → do
      t₂ ← liftCmp $ car t₂u₂
      u₂ ← liftCmp $ cdr t₂u₂
      unify t₁ t₂ >> unify u₁ u₂

    (t₁u₁, Cons t₂ u₂) → do
      t₁ ← liftCmp $ car t₁u₁
      u₁ ← liftCmp $ cdr t₁u₁
      unify t₁ t₂ >> unify u₁ u₂

    (Rigid x₁ sp₁, Rigid x₂ sp₂) | x₁ == x₂ → unifySp sp₁ sp₂
    (Flex  α₁ sp₁, Flex  α₂ sp₂) | α₁ == α₂ → unifySp sp₁ sp₂

    (Flex α sp, t) → solve α sp t
    (t, Flex α sp) → solve α sp t

    _ → throwError UnifyError

unifySp ∷ Sp → Sp → Unif ()
unifySp [] [] = return ()
unifySp (App t₁ : sp₁) (App t₂ : sp₂) = unifySp sp₁ sp₂ >> unify t₁ t₂
unifySp (Car    : sp₁) (Car    : sp₂) = unifySp sp₁ sp₂
unifySp (Cdr    : sp₁) (Cdr    : sp₂) = unifySp sp₁ sp₂
unifySp _ _ = throwError UnifyError

data Ren = Ren
  { dom ∷ Int
  , cod ∷ Int
  , map ∷ IntMap Lv
  }

keep ∷ Ren → Ren
keep (Ren dom cod map) = Ren
  { dom = 1 + dom
  , cod = 1 + cod
  , map = insert cod (Lv dom) map
  }

-- This should be in *Qu* monad!
invert ∷ Sp → Unif Ren
invert sp = do
  let go ∷ Sp → Unif (Int, IntMap Lv)
      go [] = return (0, mempty)
      go (App t : sp) = do
        (dom, map) ← go sp
        liftCmp (force t) >>= \case
          Ne (Var (Lv x)) [] | notMember x map → return $ (1 + dom, insert x (Lv dom) map)
          _ → throwError UnifyError
      go _ = throwError UnifyError

  (dom, map) ← go sp
  cod        ← asks size
  return $ Ren dom cod map

-- This should be in *Qu* monad!
rename ∷ Id → Ren → Val → Unif Tm
rename α ren t =
  liftCmp (force t) >>= \case
    Set → return T.Set

    Pi _A _B → do
      _A ← rename α ren _A
      _B ← bind $ \ x → rename α (keep ren) =<< liftCmp (resume _B x)
      return $ T.Pi _A _B

    Lam t → do
      t ← bind $ \ x → rename α (keep ren) =<< liftCmp (resume t x)
      return $ T.Lam t

    Sg _A _B → do
      _A ← rename α ren _A
      _B ← bind $ \ x → rename α (keep ren) =<< liftCmp (resume _B x)
      return $ T.Sg _A _B

    Cons t u → do
      t ← rename α ren t
      u ← rename α ren u
      return $ T.Cons t u

    Rigid (Lv x) sp → case lookup x (map ren) of
      Nothing → throwError UnifyError
      Just x  → renameSp α ren (T.Var $ lv2ix (dom ren) x) sp

    Flex β sp | α == β    → throwError UnifyError
              | otherwise → renameSp α ren (T.Meta β) sp

-- This should be in *Qu* monad!
renameSp ∷ Id → Ren → Tm → Sp → Unif Tm
renameSp _ _ t [] = return t
renameSp α ren t (App u : sp) = T.App <$> renameSp α ren t sp <*> rename α ren u
renameSp α ren t (Car   : sp) = T.Car <$> renameSp α ren t sp
renameSp α ren t (Cdr   : sp) = T.Cdr <$> renameSp α ren t sp

lams ∷ Int → Tm → Tm
lams 0 t = t
lams n t = T.Lam $ lams (n-1) t

solve ∷ Id → Sp → Val → Unif ()
solve α sp rhs = do
  ren   ← invert sp
  rhs   ← rename α ren rhs
  solut ← liftEv [] $ eval (lams (dom ren) rhs)
  α ≔ solut
