module TypeChecking.Elaboration where
import Control.Monad.RWS hiding (Sum, void)
import Data.List hiding (sum)
import Prelude hiding (pi, sum)
import Syntax.Constraint
import Syntax.NormalForm hiding (Set, Void, Unit, Sum, Pi, Sigma, Sole, Cons, Inj₁, Inj₂, Lam, Var, App, Car, Cdr, Case, ExFalso)
import Syntax.Term
import Unbound.Generics.LocallyNameless

type Elab = FreshMT (RWS Con [Heterogeneous] Sig)

runElab m = runRWS (runFreshMT m) [] []

ignoreShadowed ∷ Con → Con
ignoreShadowed = foldl
  (\ acc (x, _A) → case lookup x acc of
      Just _  → acc
      Nothing → acc ++ [(x, _A)]) []

newMeta ∷ Nf → Elab Nf
newMeta _A = do
  _Γ ← reader $ ignoreShadowed
  m  ← fresh $ s2n "?"
  modify ((m, _Γ ⟶ _A) :)
  return $ appⁿ (meta m) (vars _Γ)
    where vars ∷ Con → [Nf]
          vars = reverse . map (var . fst)

elab ∷ Tm → Nf → Elab Nf
elab t _A =
  let expect ∷ Nf → Nf → Elab Nf
      expect t _B = do
        _Γ ← ask
        m  ← newMeta _A
        tell [ _Γ ⊢ t ∶ _B ≅ m ∶ _A ]
        return m
  in
  case t of
    Set → expect set set

    Void → expect void set

    Unit → expect unit set

    Sum _S _T → do
      _S ← elab _S set
      _T ← elab _T set
      expect (sum _S _T) set

    Pi (s2n → x) _S _T → do
      _S ←                     elab _S set
      _T ← local ((x, _S) :) $ elab _T set
      expect (pi x _S _T) set

    Sigma (s2n → x) _S _T → do
      _S ←                     elab _S set
      _T ← local ((x, _S) :) $ elab _T set
      expect (sigma x _S _T) set

    Var (s2n → x) → do
      result ← reader (lookup x)
      case result of
        Just _B → expect (var x) _B
        Nothing → error $ "Unbound variable: " ++ name2String x

    Lam (s2n → x) t → do
      _S ←                     newMeta set
      _T ← local ((x, _S) :) $ newMeta set
      t  ← local ((x, _S) :) $ elab t _T
      expect (lam x t) $ pi x _S _T

    App t u → do
      x  ← fresh $ s2n "x"
      _S ←                     newMeta set
      _T ← local ((x, _S) :) $ newMeta set
      t  ← elab t $ pi x _S _T
      u  ← elab u $      _S
      expect (app t u) $ subst x u _T

    Cons t₁ t₂ → do
      x  ← fresh $ s2n "x"
      _S ←                     newMeta set
      _T ← local ((x, _S) :) $ newMeta set
      t₁ ← elab t₁ $            _S
      t₂ ← elab t₂ $ subst x t₁ _T
      expect (cons t₁ t₂) $ sigma x _S _T

    Car t → do
      x  ← fresh $ s2n "x"
      _S ←                     newMeta set
      _T ← local ((x, _S) :) $ newMeta set
      t  ← elab t $ sigma x _S _T
      expect (car t) $ _S

    Cdr t → do
      x  ← fresh $ s2n "x"
      _S ←                     newMeta set
      _T ← local ((x, _S) :) $ newMeta set
      t  ← elab t $ sigma x _S _T
      expect (cdr t) $ subst x (car t) _T

    Sole → expect sole unit

    Inj₁ t → do
      _S ← newMeta set
      _T ← newMeta set
      t  ← elab t _S
      expect (inj₁ t) $ sum _S _T

    Inj₂ t → do
      _S ← newMeta set
      _T ← newMeta set
      t  ← elab t _T
      expect (inj₂ t) $ sum _S _T

    Case t _P u₁ u₂ → do
      x  ← fresh $ s2n "x"
      _S ← newMeta set
      _T ← newMeta set
      t  ← elab t  $ sum _S _T
      _P ← elab _P $ sum _S _T ⇨ set
      u₁ ← elab u₁ $ pi x _S (app _P (inj₁ (var x)))
      u₂ ← elab u₂ $ pi x _S (app _P (inj₂ (var x)))
      expect (case' t _P u₁ u₂) $ app _P t

    ExFalso t _P → do
      t  ← elab t  $ void
      _P ← elab _P $ void ⇨ set
      expect (exFalso t _P) $ app _P t

    Hole → newMeta _A
