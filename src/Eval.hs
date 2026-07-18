module Eval where
import Prelude hiding (lookup, fst, snd)
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
  return \ t → evalIn (t:ρ) body

resume ∷ Clo Val → Val → Val
resume f t = f t

app ∷ Val → Val → Val
app t u = case t of
  V.Lam body → resume body u
  V.Stuck (ne , V.Pi 𝕒 𝕓)
    → V.Stuck (V.App ne (u , 𝕒) , resume 𝕓 u)
  _ → impossible

fst ∷ Val → Val
fst = \case
  V.Pair a _ → a
  V.Stuck (ne , V.Sg 𝕒 _)
    → V.Stuck (V.Fst ne , 𝕒)
  _ → impossible

snd ∷ Val → Val
snd t = case t of
  V.Pair _ d → d
  V.Stuck (ne , V.Sg _ 𝕓)
    → V.Stuck (V.Snd ne , resume 𝕓 (fst t))
  _ → impossible

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
  T.Tag  t   → V.Tag  <$> eval t
  T.Lam  t   → V.Lam  <$> stop t
  T.App  t u → app    <$> eval t <*> eval u
  T.Pair t u → V.Pair <$> eval t <*> eval u
  T.Fst  t   → fst    <$> eval t
  T.Snd  t   → snd    <$> eval t
  T.Sole     → return  V.Sole
  T.Tick l   → return (V.Tick l)
  T.Nil      → return  V.Nil
  T.Cons t u → V.Cons <$> eval t <*> eval u
  T.Ze       → return  V.Ze
  T.Su   t   → V.Su   <$> eval t
  T.Let  t u → resume <$> stop u <*> eval t

eval₀ ∷ Tm → Val
eval₀ = runPureEff . runReader @Env [] . eval

evalIn ∷ Env → Tm → Val
evalIn ρ = runPureEff . runReader @Env ρ . eval
