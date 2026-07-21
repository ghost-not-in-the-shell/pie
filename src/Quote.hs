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
  V.Desc  → return T.Desc
  V.Nat   → return T.Nat
  V.Tag e → T.Tag <$> quote V.Enum e

  V.Stuck (ne , V.Set) → quoteNe ne

  V.Lam _ → impossible
  V.Pair _ _ → impossible
  V.Sole → impossible
  V.Tick _ → impossible
  V.Nil → impossible
  V.Cons _ _ → impossible
  V.Ze _ _ → impossible
  V.Su _ _ _ → impossible
  V.Zero → impossible
  V.Suc _ → impossible
  V.End → impossible
  V.Arg _ _ → impossible
  V.Rec _ → impossible
  V.Stuck _ → impossible

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
    _ → error "label"

  V.Enum → case t of
    V.Nil → return T.Nil
    V.Cons l e → do
      l ← quote V.Label l
      e ← quote V.Enum  e
      return $ T.Cons l e
    V.Stuck (ne , V.Enum) → quoteNe ne
    _ → error "enum"

  V.Tag _ → case t of
    V.Ze l e → do
      l ← quote V.Label l
      e ← quote V.Enum  e
      return $ T.Ze l e
    V.Su l e t → do
      lₜ ← quote  V.Label  l
      eₜ ← quote  V.Enum   e
      tₜ ← quote (V.Tag e) t
      return $ T.Su lₜ eₜ tₜ
    V.Stuck (ne , V.Tag _) → quoteNe ne
    _ → error "tag"

  V.Nat → case t of
    V.Zero → return T.Zero
    V.Suc n → T.Suc <$> quote V.Nat n
    V.Stuck (ne , V.Nat) → quoteNe ne
    _ → error "quote nat"

  V.Desc → case t of
    V.End → return T.End
    V.Arg s d → T.Arg <$> quote V.Set s <*> quote (s `V.arrow` V.Desc) d
    V.Rec d → T.Rec <$> quote V.Desc d
    V.Stuck (ne , V.Desc) → quoteNe ne
    _ → error $ show t

  V.Stuck _ → case t of
    V.Stuck (ne , _) → quoteNe ne
    _ → error $ "stuck: " ++ show t

  _ → impossible

quoteNe ∷ Reader Len :> es ⇒ Ne → Eff es Tm
quoteNe = \case
  V.Var x → do
    n ← ask @Len
    return $ T.BVar (lv2ix n x)

  V.Fst t → T.Fst <$> quoteNe t
  V.Snd t → T.Snd <$> quoteNe t
  V.App t (u , 𝕒) → T.App <$> quoteNe t <*> quote 𝕒 u

  V.ElimEnum scrut mot nil cons →
    let motTy  = V.Enum `V.arrow` V.Set in
    let nilTy  = mot `app` V.Nil in
    let consTy = V.pi V.Label \ l →
                 V.pi V.Enum  \ e →
                 (mot `app` e) `V.arrow` (mot `app` V.Cons l e) in
    T.ElimEnum <$> quoteNe scrut
               <*> quote motTy  mot
               <*> quote nilTy  nil
               <*> quote consTy cons

  V.ElimTag scrut mot ze su →
    let motTy = V.pi  V.Enum   \ e → V.Tag e `V.arrow` V.Set in
    let zeTy  = V.pi  V.Label  \ l →
                V.pi  V.Enum   \ e →
                mot `app` V.Cons l e `app` V.Ze l e in
    let suTy  = V.pi  V.Label  \ l →
                V.pi  V.Enum   \ e →
                V.pi (V.Tag e) \ t →
                (mot `app` e `app` t) `V.arrow` (mot `app` V.Cons l e `app` V.Su l e t) in
    T.ElimTag <$> quoteNe scrut
              <*> quote motTy mot
              <*> quote zeTy ze
              <*> quote suTy su

  V.Case e p →
    let pTy = V.Tag (V.Stuck (e , V.Enum)) `V.arrow` V.Set in
    T.Case <$> quoteNe e
           <*> quote pTy p

  V.Switch e t p cs →
    let pTy = V.Tag e `V.arrow` V.Set in
    let csTy = Eval.case_ e p in
    T.Switch <$> quoteNe t
             <*> quote pTy  p
             <*> quote csTy cs

  V.El d x →
    T.El <$> quoteNe d <*> quoteTy x

conv ∷ Reader Len :> es ⇒ VTy → Val → Val → Eff es Bool
conv 𝕒 t₀ t₁ = (==) <$> quote 𝕒 t₀ <*> quote 𝕒 t₁
