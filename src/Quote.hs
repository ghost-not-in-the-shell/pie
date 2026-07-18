module Quote where
import Prelude hiding (fst, snd)
import Effectful
import Effectful.Reader.Static
import Var
import Core as T
import Val  as V
import Eval
import Impossible

fresh ∷ Reader Len :> es ⇒ VTy → Eff es Val
fresh 𝕒 = do
  n ← ask @Len
  return $ V.Stuck (V.Var (Lv (n - 1)) , 𝕒)

bind ∷ Reader Len :> es ⇒ VTy → (Val → Eff es tm) → Eff es (Bnd tm)
bind 𝕒 m = local @Len (1 +) $ Bnd <$> (fresh 𝕒 >>= m)

quoteTy ∷ Reader Len :> es ⇒ VTy → Eff es Ty
quoteTy = \case
  V.Pi 𝕒 𝕓 → do
    𝕒ₜ ← quoteTy 𝕒
    𝕓ₜ ← bind 𝕒 \ x → quoteTy (resume 𝕓 x)
    return $ T.Pi 𝕒ₜ 𝕓ₜ

  V.Sg 𝕒 𝕓 → do
    𝕒ₜ ← quoteTy 𝕒
    𝕓ₜ ← bind 𝕒 \ x → quoteTy (resume 𝕓 x)
    return $ T.Sg 𝕒ₜ 𝕓ₜ

  V.Set   → return T.Set
  V.Unit  → return T.Unit
  V.Label → return T.Label
  V.Enum  → return T.Enum
  V.Tag e → T.Tag <$> quote V.Enum e

  V.Stuck (ne , V.Set) → quoteNe ne

  _ → impossible

quote ∷ Reader Len :> es ⇒ VTy → Val → Eff es Tm
quote 𝕒 t = case 𝕒 of
  V.Set → quoteTy t

  V.Pi 𝕒 𝕓 → do
    body ← bind 𝕒 \ x → quote (resume 𝕓 x) (app t x)
    return $ T.Lam body

  V.Sg 𝕒 𝕓 → do
    let aᵥ = fst t
    let dᵥ = snd t
    aₜ ← quote         𝕒     aᵥ
    dₜ ← quote (resume 𝕓 aᵥ) dᵥ
    return $ T.Pair aₜ dₜ

  V.Unit → return T.Sole

  V.Label → case t of
    V.Tick l → return $ T.Tick l
    V.Stuck (ne , V.Label) → quoteNe ne
    _ → impossible

  V.Enum → case t of
    V.Nil → return T.Nil
    V.Cons l e → do
      l ← quote V.Label l
      e ← quote V.Enum  e
      return $ T.Cons l e
    V.Stuck (ne , V.Enum) → quoteNe ne
    _ → impossible

  V.Stuck _ → case t of
    V.Stuck (ne , _) → quoteNe ne
    _ → impossible

  _ → impossible

quoteNe ∷ Reader Len :> es ⇒ Ne → Eff es Tm
quoteNe = \case
  V.Var x → do
    n ← ask @Len
    return $ T.BVar (lv2ix n x)

  V.Fst t → T.Fst <$> quoteNe t
  V.Snd t → T.Snd <$> quoteNe t
  V.App t (u , 𝕒) → T.App <$> quoteNe t <*> quote 𝕒 u

conv ∷ Reader Len :> es ⇒ VTy → Val → Val → Eff es Bool
conv 𝕒 t₀ t₁ = (==) <$> quote 𝕒 t₀ <*> quote 𝕒 t₁
