module Eval where
import Prelude hiding (lookup, fst, snd, all)
import Effectful
import Effectful.Reader.Static
import Var
import Core as T
import Val  as V
import Impossible

lookup ∷ Reader Env :> es ⇒ Ix → Eff es Val
lookup (Ix i) = do
  ρ ← ask @Env
  return $ ρ !! i

stop ∷ Reader Env :> es ⇒ Bnd Tm → Eff es (Clo Val)
stop (Bnd body) = do
  ρ ← ask @Env
  return $ Subst (body , ρ)

resume ∷ Clo Val → Val → Val
resume (HOAS f) t = f t
resume (Subst (body , ρ)) t = evalIn (t:ρ) body

app ∷ Val → Val → Val
app t u = case t of
  V.Lam body → resume body u
  V.Stuck (ne , V.Pi 𝕒 𝕓)
    → V.Stuck (V.App ne (u , 𝕒) , resume 𝕓 u)
  _ → error "app"

fst ∷ Val → Val
fst = \case
  V.Pair a _ → a
  V.Stuck (ne , V.Sg 𝕒 _)
    → V.Stuck (V.Fst ne , 𝕒)
  _ → error "fst"

snd ∷ Val → Val
snd t = case t of
  V.Pair _ d → d
  V.Stuck (ne , V.Sg _ 𝕓)
    → V.Stuck (V.Snd ne , resume 𝕓 (fst t))
  _ → error "snd"

elimEnum ∷ Val → Val → Val → Val → Val
elimEnum scrut mot nil cons = case scrut of
  V.Nil      → nil
  V.Cons l e → cons `app` l `app` e `app` elimEnum e mot nil cons
  V.Stuck (ne , V.Enum)
    → V.Stuck (V.ElimEnum ne mot nil cons , mot `app` scrut)
  _ → error "elimEnum"

elimTag ∷ Val → Val → Val → Val → Val
elimTag scrut mot ze su = case scrut of
  V.Ze e l   → ze `app` l `app` e
  V.Su e l t → su `app` l `app` e `app` t `app` (elimTag t mot ze su)
  V.Stuck (ne , V.Tag e)
    → V.Stuck (V.ElimTag ne mot ze su , mot `app` e `app` scrut)
  _ → error "elimTag"

all ∷ Val → Val → Val → Val → Val → Val
all d x p ϕ xs = case d of
  V.End     → V.Sole
  V.Arg s d →                             all (d `app` s) x p ϕ (snd xs)
  V.Rec   d → (ϕ `app` (fst xs)) `V.Pair` all  d          x p ϕ (snd xs)
  V.Stuck (ne , V.Desc)
    → V.Stuck (V.All ne x p ϕ xs , V.Stuck (V.Hyps ne x p xs , V.Set))
  _ → error "all"

elim ∷ Val → Val → Val → Val
elim scrut p ϕ = case scrut of
  V.Inj ds → ϕ `app` ds `app` all scrut (V.Mu scrut) p (elim scrut p ϕ) ds
  V.Stuck (ne , V.Mu d)
    → V.Stuck (V.Elim d ne p ϕ , p `app` scrut)
  _ → error "elim"

case_ ∷ Val → Val → Val
case_ e p = case e of
  V.Nil → V.Unit
  V.Cons l e → (p `app` V.Ze l e) `V.prod` case_ e (V.lam \ t → p `app` V.Su l e t)
  V.Stuck (ne , V.Enum)
    → V.Stuck (V.Case ne p , V.Set)
  _ → error "case_"

switch ∷ Val → Val → Val → Val
switch t p cs = case t of
  V.Ze _ _ → fst cs
  V.Su l e t → switch t (V.lam \ t → p `app` V.Su l e t) (snd cs)
  V.Stuck (ne , V.Tag e)
    → V.Stuck (V.Switch e ne p cs , p `app` t)
  t → error $ "switch: " ++ show t

decode ∷ Val → Val → Val
decode d x = case d of
  V.End → V.Unit
  V.Arg s d → V.sg s \ s →    decode (d `app` s) x
  V.Rec   d →      x `V.prod` decode  d          x
  V.Stuck (ne , V.Desc)
    → V.Stuck (V.El ne x , V.Set)
  _ → error "decode"

hyps ∷ Val → Val → Val → Val → Val
hyps d x p xs = case d of
  V.End     → V.Unit
  V.Arg s d →                           hyps (d `app` s) x p (snd xs)
  V.Rec   d → p `app` (fst xs) `V.prod` hyps  d          x p (snd xs)
  V.Stuck (ne , V.Desc)
    → V.Stuck (V.Hyps ne x p xs , V.Set)
  _ → error "hyps"

eval ∷ Reader Env :> es ⇒ Tm → Eff es Val
eval = \case
  T.FVar _ → impossible
  T.BVar x → lookup x
  T.Pi 𝕒 𝕓 → V.Pi <$> eval 𝕒 <*> stop 𝕓
  T.Sg 𝕒 𝕓 → V.Sg <$> eval 𝕒 <*> stop 𝕓
  T.Set   → return V.Set  
  T.Unit  → return V.Unit
  T.Label → return V.Label
  T.Enum  → return V.Enum
  T.Desc  → return V.Desc
  T.Nat   → return V.Nat
  T.Tag  t   → V.Tag  <$> eval t
  T.Lam  t   → V.Lam  <$> stop t
  T.App  t u → app    <$> eval t <*> eval u
  T.Pair t u → V.Pair <$> eval t <*> eval u
  T.Fst  t   → fst    <$> eval t
  T.Snd  t   → snd    <$> eval t
  T.Sole     → return  V.Sole
  T.Zero     → return  V.Zero
  T.Suc  t   → V.Suc  <$> eval t
  T.Tick l   → return (V.Tick l)
  T.Nil      → return  V.Nil
  T.Cons t u → V.Cons <$> eval t <*> eval u
  T.End      → return  V.End
  T.Arg  s d → V.Arg  <$> eval s <*> eval d
  T.Rec  d   → V.Rec  <$> eval d
  T.Mu   d   → V.Mu   <$> eval d
  T.Inj  ϕ   → V.Inj  <$> eval ϕ
  T.Ze l e   → V.Ze   <$> eval l <*> eval e
  T.Su l e t → V.Su   <$> eval l <*> eval e <*> eval t
  T.Let  t u → resume <$> stop u <*> eval t
  T.All d x p ϕ xs →
    all <$> eval d
        <*> eval x
        <*> eval p
        <*> eval ϕ
        <*> eval xs
  T.Elim scrut p ϕ →
    elim <$> eval scrut
         <*> eval p
         <*> eval ϕ
  T.ElimEnum scrut mot nil cons →
    elimEnum <$> eval scrut
             <*> eval mot
             <*> eval nil
             <*> eval cons
  T.ElimTag scrut mot ze su →
    elimTag <$> eval scrut
            <*> eval mot
            <*> eval ze
            <*> eval su
  T.Case e p → case_  <$> eval e <*> eval p
  T.Switch t p cs →
    switch <$> eval t
           <*> eval p
           <*> eval cs
  T.El d x → decode <$> eval d <*> eval x
  T.Hyps d x p ϕ → hyps <$> eval d <*> eval x <*> eval p <*> eval ϕ

eval₀ ∷ Tm → Val
eval₀ = runPureEff . runReader @Env [] . eval

evalIn ∷ Env → Tm → Val
evalIn ρ = runPureEff . runReader @Env ρ . eval
