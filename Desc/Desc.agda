module Desc.Desc where
open import Desc.Prelude renaming (String to Label)

data Desc : Set₁ where
  end :                            Desc
  arg : (S : Set) (𝔻 : S → Desc) → Desc
  rec :           (𝔻 :     Desc) → Desc

elimDesc : ∀ {ℓ} (P : Desc → Set ℓ)
  → P end
  → ((S : Set) (𝔻 : S → Desc) (ind : (s : S) → P (𝔻 s)) → P (arg S 𝔻))
  → ((𝔻 : Desc) (ind : P 𝔻) → P (rec 𝔻))
  → (𝔻 : Desc) → P 𝔻
elimDesc P ecase acase rcase (end    ) =
  ecase
elimDesc P ecase acase rcase (arg S 𝔻) =
  acase S 𝔻 (λ s → elimDesc P ecase acase rcase (𝔻 s))
elimDesc P ecase acase rcase (rec   𝔻) =
  rcase   𝔻 (      elimDesc P ecase acase rcase  𝔻   )

⟦_⟧ : ∀ {ℓ} → Desc → Set ℓ → Set ℓ
⟦ end     ⟧ X = ⊤
⟦ arg S 𝔻 ⟧ X = [ s ∈ S ] × ⟦ 𝔻 s ⟧ X
⟦ rec   𝔻 ⟧ X =       X   × ⟦ 𝔻   ⟧ X

Hyps : ∀ {ℓ} (𝔻 : Desc) (X : Set) (P : X → Set ℓ)
  → (xs : ⟦ 𝔻 ⟧ X) → Set ℓ
Hyps (end    ) X P tt       = ⊤
Hyps (arg S 𝔻) X P (s , xs) =       Hyps (𝔻 s) X P xs
Hyps (rec   𝔻) X P (x , xs) = P x × Hyps  𝔻    X P xs

all : ∀ {ℓ} (𝔻 : Desc) (X : Set) (P : X → Set ℓ)
  → ((x : X) → P x)
  → (xs : ⟦ 𝔻 ⟧ X)
  → Hyps 𝔻 X P xs
all (end    ) X P ϕ xs       = tt
all (arg S 𝔻) X P ϕ (s , xs) =       all (𝔻 s) X P ϕ xs
all (rec   𝔻) X P ϕ (x , xs) = ϕ x , all  𝔻    X P ϕ xs

data μ (𝔻 : Desc) : Set where
  ⟨_⟩ : ⟦ 𝔻 ⟧ (μ 𝔻) → μ 𝔻

{-# TERMINATING #-}
ind : ∀ {ℓ} {𝔻 : Desc} (P : μ 𝔻 → Set ℓ)
  → ((ds : ⟦ 𝔻 ⟧ (μ 𝔻)) → Hyps 𝔻 (μ 𝔻) P ds → P ⟨ ds ⟩)
  → (d : μ 𝔻) → P d
ind {𝔻 = 𝔻} P ϕ ⟨ ds ⟩ = ϕ ds (all 𝔻 (μ 𝔻) P (ind {𝔻 = 𝔻} P ϕ) ds)

map : (𝔻 : Desc) {X Y : Set} → (X → Y) → ⟦ 𝔻 ⟧ X → ⟦ 𝔻 ⟧ Y
map (end    ) f tt       = tt
map (arg S 𝔻) f (s , xs) = s   , map (𝔻 s) f xs
map (rec   𝔻) f (x , xs) = f x , map  𝔻    f xs

{-# TERMINATING #-}
fold : {𝔻 : Desc} {X : Set}
  → (⟦ 𝔻 ⟧ X → X)
  → μ 𝔻 → X
fold {𝔻} ϕ ⟨ ds ⟩ = ϕ (map 𝔻 (fold ϕ) ds)

module Original where
  Enum : Set
  Enum = List Label

  elimEnum : ∀ {ℓ} (P : Enum → Set ℓ)
    → P []
    → ((l : Label) (𝔼 : Enum) → P 𝔼 → P (l ∷ 𝔼))
    → (𝔼 : Enum) → P 𝔼
  elimEnum P nil cons []      = nil
  elimEnum P nil cons (l ∷ 𝔼) = cons l 𝔼 (elimEnum P nil cons 𝔼)

  data Tag : Enum → Set where
    ze : ∀ {l 𝔼}         → Tag (l ∷ 𝔼)
    su : ∀ {l 𝔼} → Tag 𝔼 → Tag (l ∷ 𝔼)

  elimTag : ∀ {ℓ} (mot : ∀ 𝔼 → Tag 𝔼 → Set ℓ)
    → (∀ l 𝔼 → mot (l ∷ 𝔼) ze)
    → (∀ l 𝔼 (t : Tag 𝔼) → mot 𝔼 t → mot (l ∷ 𝔼) (su t))
    → ∀ {𝔼} (t : Tag 𝔼) → mot 𝔼 t
  elimTag P zcase scase {l ∷ 𝔼}  ze    = zcase l 𝔼
  elimTag P zcase scase {l ∷ 𝔼} (su t) = scase l 𝔼 t (elimTag P zcase scase t)

  Case : ∀ {ℓ} (𝔼 : Enum) (P : Tag 𝔼 → Set ℓ) → Set ℓ
  Case []      P = ⊤
  Case (l ∷ 𝔼) P = P ze × Case 𝔼 (P ∘ su)

  switch : ∀ {ℓ} {𝔼 : Enum} (P : Tag 𝔼 → Set ℓ)
    → (cs : Case 𝔼 P)
    → (t : Tag 𝔼) → P t
  switch {𝔼 = l ∷ 𝔼} P cs  ze    = fst cs
  switch {𝔼 = l ∷ 𝔼} P cs (su t) = switch (P ∘ su) (snd cs) t

  Case′ : ∀ {ℓ} (𝔼 : Enum) (P : Tag 𝔼 → Set ℓ) → Set ℓ
  Case′ {ℓ} 𝔼 P = elimEnum
    (λ 𝔼 → (Tag 𝔼 → Set ℓ) → Set ℓ)
    (λ P → ⊤)
    (λ l 𝔼 ind P → P (ze {l} {𝔼}) × ind (λ t → P (su {l} {𝔼} t)))
    𝔼 P

  switch′ : ∀ {ℓ} {𝔼 : Enum} (P : Tag 𝔼 → Set ℓ)
    → (cs : Case′ 𝔼 P)
    → (t : Tag 𝔼) → P t
  switch′ {ℓ} P cs t = elimTag
    (λ 𝔼 t → (P : Tag 𝔼 → Set ℓ) → Case′ 𝔼 P → P t)
    (λ l 𝔼 P (c , cs) → c)
    (λ l 𝔼 t ind P (c , cs) → ind (P ∘ su {l} {𝔼}) cs)
    t
    P
    cs

  [nil,cons] : Enum
  [nil,cons] = "nil" ∷ "cons" ∷ []

  pattern ‘nil  = ze
  pattern ‘cons = su ze

  nilD : Desc
  nilD = end

  consD : Desc
  consD = arg Label $ const $ rec end

  nilD+consD : Tag [nil,cons] → Desc
  nilD+consD = switch (const Desc)
    ( nilD
    , consD
    , tt )

  EnumD : Desc
  EnumD = arg (Tag [nil,cons]) nilD+consD

  [ze,su] : Enum
  [ze,su] = "ze" ∷ "su" ∷ []

  pattern ‘ze = ze
  pattern ‘su = su ze

  zeD : Desc
  zeD = end

  suD : Desc
  suD = rec end

  NatC : Tag [ze,su] → Desc
  NatC = switch (const Desc)
    ( zeD
    , suD
    , tt )

  NatD : Desc
  NatD = arg (Tag [ze,su]) NatC

  Nat : Set
  Nat = μ NatD

  Toℕ : Nat → Set
  Toℕ = const ℕ

  toℕ : Nat → ℕ
  toℕ = ind Toℕ $ curry {A = Tag [ze,su]}
                        {λ t → ⟦ NatC t ⟧ Nat}
                       {λ t → Hyps NatD Nat Toℕ t → Toℕ ⟨ t ⟩} $
    switch (λ t → (ds : ⟦ NatC t ⟧ Nat) → Hyps NatD Nat Toℕ (t , ds) → Toℕ ⟨ t , ds ⟩)
      ( (λ _ _ → zero)
      , (λ n hyps → suc (fst hyps))
      , tt
      )
-- curry f p = f (fst p , snd p)

  zeBranch : (ds : ⟦ zeD ⟧ Nat) (ihs : Hyps zeD Nat Toℕ ds) → Toℕ ⟨ ‘ze , ds ⟩
  zeBranch = λ { tt tt → zero }

  suBranch : (ds : ⟦ suD ⟧ Nat) (ihs : Hyps suD Nat Toℕ ds) → Toℕ ⟨ ‘su , ds ⟩
  suBranch = λ ds ihs → suc (fst ihs)

  Toℕ′ : Nat → ℕ
  Toℕ′ = ind Toℕ $ λ p → switch (λ t → (ds : ⟦ NatC t ⟧ Nat) (ihs : Hyps NatD Nat Toℕ (t , ds)) → Toℕ ⟨ t , ds ⟩)
    ( zeBranch
    , suBranch
    , tt) (fst p) (snd p)

  Toℕα : (ds : ⟦ NatD ⟧ Nat) (ihs : Hyps NatD Nat Toℕ ds) → Toℕ ⟨ ds ⟩
  Toℕα ds = switch (λ t → (ds : ⟦ NatC t ⟧ Nat) (ihs : Hyps NatD Nat Toℕ (t , ds)) → Toℕ ⟨ t , ds ⟩)
    ({!!} , (λ ds ihs → suc (fst ihs)) , tt) (fst ds) (snd ds)

  Toℕ″ : Nat → ℕ
  Toℕ″ = ind Toℕ Toℕα

  pattern ze_ = ⟨ ‘ze , tt ⟩
  pattern su_ n = ⟨ ‘su , n , tt ⟩

  three : Nat
  three = su_ (su_ (su_ ze_))

  _ : toℕ three ≡ 3
  _ = refl

module Jiawei where
  Enum : Set
  Enum = List Label

  recEnum : ∀ {ℓ} {X : Set ℓ}
    → X
    → (Label → X → X)
    → Enum → X
  recEnum n c []      = n
  recEnum n c (l ∷ 𝔼) = c l (recEnum n c 𝔼)

  data Tag : Enum → Set where
    ze : ∀ {l 𝔼}         → Tag (l ∷ 𝔼)
    su : ∀ {l 𝔼} → Tag 𝔼 → Tag (l ∷ 𝔼)

  recTag : ∀ {ℓ} (X : Enum → Set ℓ)
    → (∀ {l 𝔼}       → X (l ∷ 𝔼))
    → (∀ {l 𝔼} → X 𝔼 → X (l ∷ 𝔼))
    → {𝔼 : Enum} → Tag 𝔼 → X 𝔼
  recTag X z s  ze    = z
  recTag X z s (su t) = s (recTag X z s t)

  Case : (𝔼 : Enum) → Set₁
  Case []      = ⊤
  Case (l ∷ 𝔼) = Desc × Case 𝔼

  -- this is just algebra of Enum
  Case'' : ∀ {ℓ} (𝔼 : Enum) (X : Set ℓ) → Set (lsuc ℓ)
  Case'' []      X = ⊤
  Case'' (l ∷ 𝔼) X = X × Case'' 𝔼 X

  Case' : (𝔼 : Enum) → Set₁
  Case' = recEnum ⊤ (λ _ → Desc ×_)

  switch : {𝔼 : Enum}
    → Case 𝔼
    → Tag  𝔼 → Desc
  switch (c , cs)  ze    = c
  switch (c , cs) (su t) = switch cs t

  switch'' : ∀ {ℓ} {𝔼 : Enum} {X : Set ℓ}
    → Case'' 𝔼 X
    → Tag 𝔼 → X
  switch'' (c , cs)  ze    = c
  switch'' (c , cs) (su t) = switch'' cs t

  switch' : {𝔼 : Enum}
    → Case' 𝔼
    → Tag   𝔼 → Desc
  switch' cs t = recTag (λ 𝔼 → Case' 𝔼 → Desc)
    (λ cs → fst cs)
    (λ ih cs → ih (snd cs))
    t cs

  [nil,cons] : Enum
  [nil,cons] = "nil" ∷ "cons" ∷ []

  pattern ‘nil  = ze
  pattern ′cons = su ze

  nilD : Desc
  nilD = end

  consD : Desc
  consD = arg Label $ const $ rec end

  nilD+consD : Tag [nil,cons] → Desc
  nilD+consD = switch''
    ( nilD
    , consD
    , tt )

  EnumD : Desc
  EnumD = arg (Tag [nil,cons]) nilD+consD

  [ze,su] : Enum
  [ze,su] = "ze" ∷ "su" ∷ []

  pattern ‘ze = ze
  pattern ‘su = su ze

  zeD : Desc
  zeD = end

  suD : Desc
  suD = rec end

  zeD+suD : Tag [ze,su] → Desc
  zeD+suD = switch
    ( zeD
    , suD
    , tt )

  NatD : Desc
  NatD = arg (Tag [ze,su]) $ zeD+suD

  Nat : Set
  Nat = μ NatD

{-
  toℕ : Nat → ℕ
  toℕ = fold $ λ { (‘ze , t) → {!!}
                 ; (‘su , y) → {!!} }

  Toℕ : Nat → Set
  Toℕ = λ _ → ℕ

  toℕ' : Nat → ℕ
  toℕ' = ind NatD Toℕ λ { (‘ze , t) → {!!}
                        ; (‘su , y) → {!!} }
-}
