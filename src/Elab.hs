module Elab where
import Prelude hiding (lookup)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Var
import Raw  as R
import Core as T
import Val  as V
import Eval
import Quote (fresh, quoteTy, quote, conv)

type Ctx = [(Name , VTy)]

data Msg = OutOfScope
         | NotAFunction Raw
         | NotAPair     Raw
         | CantInfer    Raw
         | RequireNonEmpty
         | TagNotInEnum
         | TypeMismatch
         deriving (Eq, Show)

find ∷ (a → Bool) → [a] → Maybe (Int , a)
find f xs = go 0 xs
  where go acc = \case
          [] → Nothing
          (x:xs) | f x       → Just (acc , x)
                 | otherwise → go (1+acc) xs

var ∷ ( Reader Ctx :> es
      , Error  Msg :> es
      ) ⇒ Name → Eff es (Tm , VTy)
var name = do
  γ ← ask @Ctx
  case find (\(x , _)  → x == name) γ of
    Just (i , (_ , 𝕒)) → return (T.BVar (Ix i) , 𝕒)
    Nothing → throwError OutOfScope

bind ∷ ( Reader Len :> es
       , Reader Ctx :> es
       , Reader Env :> es
       ) ⇒ Name → VTy → (Val → Eff es tm) → Eff es (Bnd tm)
bind name 𝕒 m = local @Len (1 +) $ fresh 𝕒 >>= \ x →
                local @Env (x :)
              $ local @Ctx ((name , 𝕒) :) $ Bnd <$> m x

def ∷ ( Reader Len :> es
      , Reader Ctx :> es
      , Reader Env :> es
      ) ⇒ Name → VTy → Val → Eff es tm → Eff es tm
def name 𝕒 t = local @Len (1 +)
             . local @Env (t :)
             . local @Ctx ((name , 𝕒) :)

infer ∷ ( Reader Len :> es
        , Reader Ctx :> es
        , Reader Env :> es
        , Error  Msg :> es
        ) ⇒ Raw → Eff es (Tm , VTy)
infer = \case
  R.Var name → var name

  R.Pi name 𝕒 𝕓 → do
    𝕒  ← check 𝕒 V.Set
    𝕒ᵥ ← eval  𝕒
    𝕓  ← bind name 𝕒ᵥ \ _ → check 𝕓 V.Set
    return (T.Pi 𝕒 𝕓 , V.Set)

  R.Sg name 𝕒 𝕓 → do
    𝕒  ← check 𝕒 V.Set
    𝕒ᵥ ← eval  𝕒
    𝕓  ← bind name 𝕒ᵥ \ _ → check 𝕓 V.Set
    return (T.Sg 𝕒 𝕓 , V.Set)

  R.Set   → return (T.Set   , V.Set)
  R.Unit  → return (T.Unit  , V.Set)
  R.Label → return (T.Label , V.Set)
  R.Enum  → return (T.Enum  , V.Set)
  R.Nat   → return (T.Nat   , V.Set)
  R.Desc  → return (T.Desc  , V.Set)

  R.Tag t → do
    t ← check t V.Enum
    return (T.Tag t , V.Set)

  t@(R.Lam _ Nothing _) → throwError $ CantInfer t

  R.Lam name (Just 𝕒) body → do
    𝕒  ← check 𝕒 V.Set
    𝕒ᵥ ← eval  𝕒
    Bnd (body , 𝕓) ← bind name 𝕒ᵥ \ _ → do
      (body , 𝕓ᵥ) ← infer body
      𝕓 ← quoteTy 𝕓ᵥ
      return (body , 𝕓)
    ty ← eval $ T.Pi 𝕒 (Bnd 𝕓)
    return (T.Lam (Bnd body) , ty)

  R.App t u → do
    infer t >>= \case
      (t , V.Pi 𝕒 𝕓) → do
        u ← check u 𝕒
        𝕓 ← resume 𝕓 <$> eval u
        return (T.App t u , 𝕓)
      _ → throwError $ NotAFunction t

  t@(R.Pair _ _) → throwError $ CantInfer t

  R.Fst t → do
    infer t >>= \case
      (t , V.Sg 𝕒 _) → return (T.Fst t , 𝕒)
      _ → throwError $ NotAPair t

  R.Snd t → do
    infer t >>= \case
      (t , V.Sg _ 𝕓) → do
        𝕓 ← resume 𝕓 <$> eval (T.Fst t)
        return (T.Snd t , 𝕓)
      _ → throwError $ NotAPair t

  R.Bracket [] → return (T.Sole , V.Unit)

  t@(R.Bracket (_ : _)) → throwError $ CantInfer t

  R.Tick l → return (T.Tick l , V.Label)

  R.Nil → return (T.Nil , V.Enum)
  R.Cons l e → do
    l ← check l V.Label
    e ← check e V.Enum
    return (T.Cons l e , V.Enum)

  R.Brace [] → return (T.Nil , V.Enum)

  R.Brace (l : e) → do
    l ← check l           V.Label
    e ← check (R.Brace e) V.Enum
    return (T.Cons l e , V.Enum)

  R.ElimEnum scrut mot nil cons → do
    scrut  ← check scrut V.Enum
    scrutᵥ ← eval  scrut
    let motTy = V.Enum `V.arrow` V.Set
    mot  ← check mot motTy
    motᵥ ← eval  mot
    let nilTy  = motᵥ `app` V.Nil
    let consTy = V.pi V.Label \ l →
                 V.pi V.Enum  \ e →
                 (motᵥ `app` e) `V.arrow` (motᵥ `app` V.Cons l e)
    nil  ← check nil  nilTy
    cons ← check cons consTy
    return (T.ElimEnum scrut mot nil cons , motᵥ `app` scrutᵥ)

  R.Zero → return (T.Zero , V.Nat)
  R.Suc n → do
    n ← check n V.Nat
    return (T.Suc n , V.Nat)

  R.End → return (T.End , V.Desc)

  R.Arg s d → do
    sₜ ← check s  V.Set
    sᵥ ← eval  sₜ
    d  ← check d (sᵥ `V.arrow` V.Desc)
    return (T.Arg sₜ d , V.Desc)

  R.Rec d → do
    d ← check d V.Desc
    return (T.Rec d , V.Desc)

  t@(R.Ze     ) → throwError $ CantInfer t
  t@(R.Su    _) → throwError $ CantInfer t
  t@(R.Sharp _) → throwError $ CantInfer t

  R.ElimTag scrut mot ze su →
    infer scrut >>= \case
      (scrut , V.Tag e) → do
        scrutᵥ ← eval scrut
        let motTy = V.pi V.Enum \ e -> V.Tag e `V.arrow` V.Set
        mot  ← check mot motTy
        motᵥ ← eval  mot
        let zeTy = V.pi V.Label \ l →
                   V.pi V.Enum  \ e →
                   motᵥ `app` V.Cons l e `app` V.Ze l e
        let suTy = V.pi  V.Label  \ l →
                   V.pi  V.Enum   \ e →
                   V.pi (V.Tag e) \ t →
                   (motᵥ `app` e `app` t) `V.arrow` (motᵥ `app` V.Cons l e `app` V.Su l e t)
        ze ← check ze zeTy
        su ← check su suTy
        return (T.ElimTag scrut mot ze su , motᵥ `app` e `app` scrutᵥ)
      _ → throwError $ TypeMismatch

  R.Case e p → do
    eₜ ← check e V.Enum
    eᵥ ← eval  eₜ
    p  ← check p (V.Tag eᵥ `V.arrow` V.Set)
    return (T.Case eₜ p , V.Set)

  R.Switch t p branches → do
    let eᵣ = R.Brace   $ map (R.Tick . Prelude.fst) branches
    let ϕᵣ = R.Bracket $ map           Prelude.snd  branches
    eₜ ← check eᵣ  V.Enum
    eᵥ ← eval  eₜ
    tₜ ← check t  (V.Tag eᵥ)
    tᵥ ← eval  tₜ
    pₜ ← check p  (V.Tag eᵥ `V.arrow` V.Set)
    pᵥ ← eval  pₜ
    ϕₜ ← check ϕᵣ (Eval.case_ eᵥ pᵥ)
    return (T.Switch tₜ pₜ ϕₜ , pᵥ `app` tᵥ)

  R.Let name 𝕒 t body → do
    𝕒  ← check 𝕒 V.Set
    𝕒ᵥ ← eval  𝕒
    t  ← check t 𝕒ᵥ
    tᵥ ← eval  t
    (body , 𝕓) ← def name 𝕒ᵥ tᵥ $ infer body
    return (T.Let t (Bnd body) , 𝕓)

check ∷ ( Reader Len :> es
        , Reader Ctx :> es
        , Reader Env :> es
        , Error  Msg :> es
        ) ⇒ Raw → VTy → Eff es Tm
check t expected = case (t , expected) of
  (R.Lam name _ body , V.Pi 𝕒 𝕓) → do
    body ← bind name 𝕒 \ x → check body (resume 𝕓 x)
    return $ T.Lam body

  (R.Pair t u , V.Sg 𝕒 𝕓) → do
    t ← check t 𝕒
    u ← check u =<< (resume 𝕓 <$> eval t) 
    return $ T.Pair t u

  (R.Bracket (t : ts) , V.Sg 𝕒 𝕓) → do
    t  ← check t 𝕒
    ts ← check (R.Bracket ts) =<< (resume 𝕓 <$> eval t)
    return $ T.Pair t ts

  (R.Ze , V.Tag e) → case e of
    V.Cons l e → T.Ze <$> quote V.Label l <*> quote V.Enum e
    _ → throwError RequireNonEmpty

  (R.Su t , V.Tag e) → case e of
    V.Cons l e → T.Su <$> quote V.Label l <*> quote V.Enum e <*> check t (V.Tag e)
    _ → throwError RequireNonEmpty

  (R.Sharp l , V.Tag e) → case e of
    V.Cons (V.Tick this) e → if l == this
                    then T.Ze (T.Tick this) <$> quote V.Enum e
                    else T.Su (T.Tick this) <$> quote V.Enum e <*> check (R.Sharp l) (V.Tag e)
    _ → throwError TagNotInEnum

  _ → do
    (t , actual) ← infer t
    conv V.Set expected actual >>= \case
      True  → return t
      False → throwError TypeMismatch

runElab₀ ∷ Eff [Reader Env, Reader Ctx, Reader Len, Error e] a → Either e a
runElab₀ = runPureEff
         . runErrorNoCallStack
         . runReader @Len 0
         . runReader @Ctx []
         . runReader @Env []

norm₀ ∷ Raw → Either Msg Tm
norm₀ t = runElab₀ do
  (t , 𝕒) ← infer t
  quote 𝕒 (eval₀ t)
