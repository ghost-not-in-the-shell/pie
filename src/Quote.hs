module Quote where
import Control.Monad.Reader
import Data.IntMap hiding (size)
import Debruijn
import MetaContext
import Tm  as T
import Val as V
import Eval hiding (bind, liftCmp)

data QuR = QuR { mcon ∷ IntMap MetaEntry, size ∷ Int }

type Qu = Reader QuR

topVar ∷ Qu Val
topVar = do
  n ← asks size
  return $ var $ Lv $ n-1

bind ∷ (Val → Qu a) → Qu a
bind k = local (\ con → con { size = 1 + size con } ) $ k =<< topVar

runQu ∷ Qu a → QuR → a
runQu = runReader

liftCmp ∷ Cmp a → Qu a
liftCmp m = do
  QuR { mcon = mcon } ← ask
  return $ runCmp m (CmpR { mcon = mcon })

quote ∷ Val → Qu Tm
quote t = liftCmp (force t) >>= \case
  V.Set → return T.Set

  V.Pi _A _B → do
    _A ← quote _A
    _B ← bind $ \ x → quote =<< liftCmp (resume _B x)
    return $ T.Pi _A _B

  V.Lam t → do
    t ← bind $ \ x → quote =<< liftCmp (resume t x)
    return $ T.Lam t

  V.Sg _A _B → do
    _A ← quote _A
    _B ← bind $ \ x → quote =<< liftCmp (resume _B x)
    return $ T.Sg _A _B

  V.Cons t u → do
    t ← quote t
    u ← quote u
    return $ T.Cons t u

  V.Ne hd sp → do
    hd ← quoteHd hd
    quoteSp hd sp

quoteHd ∷ Hd → Qu Tm
quoteHd = \case
  V.Var x → do
    n ← size <$> ask
    return $ T.Var $ lv2ix n x
  V.Meta α → return $ T.Meta α

quoteSp ∷ Tm → Sp → Qu Tm
quoteSp t [] = return t
quoteSp t (V.App u : sp) = T.App <$> quoteSp t sp <*> quote u
quoteSp t (V.Car   : sp) = T.Car <$> quoteSp t sp
quoteSp t (V.Cdr   : sp) = T.Cdr <$> quoteSp t sp
