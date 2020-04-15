module Syntax.Pretty where
import Syntax.Constraint
import Syntax.NormalForm
import Text.PrettyPrint
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Unsafe

class Pretty a where
  pretty ∷ a → Doc

  pp ∷ a → String
  pp = render . pretty

instance Pretty (Name a) where
  pretty = text . show

instance Pretty Nf where
  pretty Set  = text "Set"
  pretty Void = text "⊥"
  pretty Unit = text "⊤"
  pretty (Sum _S _T) =
    parens $ sep [text "∨", pretty _S, pretty _T]
  pretty (Pi (unsafeUnbind → ((x, unembed → _S), _T))) =
    parens $ sep [text "∀", parens (pretty x <+> pretty _S), pretty _T]
  pretty (Sigma (unsafeUnbind → ((x, unembed → _S), _T))) =
    parens $ sep [text "∃", parens (pretty x <+> pretty _S), pretty _T]
  pretty Sole = text "sole"
  pretty (Cons t₁ t₂) = parens $ sep [text "cons", pretty t₁, pretty t₂]
  pretty (Inj₁ t) = parens $ text "inj₁" <+> pretty t
  pretty (Inj₂ t) = parens $ text "inj₂" <+> pretty t
  pretty (Lam (unsafeUnbind → (x, t))) =
    parens $ sep [text "λ", parens (pretty x), pretty t]
  pretty (Ne h sp) =
    foldl elimPretty (pretty h) sp
    where elimPretty acc (App u) = parens $ acc <+> pretty u
          elimPretty acc (Car  ) = parens $ text "car" <+> acc
          elimPretty acc (Cdr  ) = parens $ text "cdr" <+> acc
          elimPretty acc (Case    _P u₁ u₂) = parens $ sep [text "case"    , pretty _P, acc, pretty u₁, pretty u₂]
          elimPretty acc (ExFalso _P      ) = parens $ sep [text "ex-falso", pretty _P, acc                      ]

instance Pretty Hd where
  pretty (Var  x) = pretty x
  pretty (Meta m) = pretty m

instance Pretty [(Name Nf, Nf)] where
  pretty (reverse → ρ) = sep . punctuate comma $ map (\ (x, _A) → pretty x <+> colon <+> pretty _A) ρ

instance Pretty Heterogeneous where
  pretty (_Γ, ((t₁, _A₁), (t₂, _A₂))) =
    sep [pretty _Γ, char '⊢', pretty t₁, colon, pretty _A₁, char '≅', pretty t₂, colon, pretty _A₂]

instance Pretty [Heterogeneous] where
  pretty cs = brackets . sep . punctuate comma $ map pretty cs

instance Pretty Homogeneous where
  pretty (_Γ, ((t₁, t₂), _A)) =
    sep [pretty _Γ, char '⊢', pretty t₁, char '≡', pretty t₂, colon, pretty _A]

instance Pretty [Homogeneous] where
  pretty cs = brackets . sep . punctuate comma $ map pretty cs
