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

All : ∀ {ℓ} (𝔻 : Desc) (X : Set) (P : X → Set ℓ)
  → (xs : ⟦ 𝔻 ⟧ X) → Set ℓ
All (end    ) X P tt       = ⊤
All (arg S 𝔻) X P (s , xs) =       All (𝔻 s) X P xs
All (rec   𝔻) X P (x , xs) = P x × All  𝔻    X P xs

all : ∀ {ℓ} (𝔻 : Desc) (X : Set) (P : X → Set ℓ)
  → ((x : X) → P x)
  → (xs : ⟦ 𝔻 ⟧ X) → All 𝔻 X P xs
all (end    ) X P ϕ tt       = tt
all (arg S 𝔻) X P ϕ (s , xs) =       all (𝔻 s) X P ϕ xs
all (rec   𝔻) X P ϕ (x , xs) = ϕ x , all  𝔻    X P ϕ xs

data μ (𝔻 : Desc) : Set where
  ⟨_⟩ : ⟦ 𝔻 ⟧ (μ 𝔻) → μ 𝔻

{-# TERMINATING #-}
ind : ∀ {ℓ} (𝔻 : Desc) (P : μ 𝔻 → Set ℓ)
  → ((ds : ⟦ 𝔻 ⟧ (μ 𝔻)) → All 𝔻 (μ 𝔻) P ds → P ⟨ ds ⟩)
  → (d : μ 𝔻) → P d
ind 𝔻 P ϕ ⟨ ds ⟩ = ϕ ds (all 𝔻 (μ 𝔻) P (ind 𝔻 P ϕ) ds)

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

  elimTag : ∀ {ℓ} (mot : ∀ {𝔼} → Tag 𝔼 → Set ℓ)
    → (∀ {l 𝔼} → mot {l ∷ 𝔼} ze)
    → (∀ {l 𝔼} (t : Tag 𝔼) → mot {𝔼} t → mot {l ∷ 𝔼} (su t))
    → ∀ {𝔼} (t : Tag 𝔼) → mot {𝔼} t
  elimTag P zcase scase {l ∷ 𝔼}  ze    = zcase
  elimTag P zcase scase {l ∷ 𝔼} (su t) = scase t (elimTag P zcase scase t)

  Case : ∀ {ℓ} (𝔼 : Enum) (P : Tag 𝔼 → Set ℓ) → Set ℓ
  Case []      P = ⊤
  Case (l ∷ 𝔼) P = P ze × Case 𝔼 (P ∘ su)

  case : ∀ {ℓ} (𝔼 : Enum) (P : Tag 𝔼 → Set ℓ)
    → (cs : Case 𝔼 P)
    → (t : Tag 𝔼) → P t
  case (l ∷ 𝔼) P (c , cs)  ze    = c
  case (l ∷ 𝔼) P (c , cs) (su t) = case 𝔼 (P ∘ su) cs t

  Case′ : ∀ {ℓ} (𝔼 : Enum) (P : Tag 𝔼 → Set ℓ) → Set ℓ
  Case′ {ℓ} = elimEnum
    (λ 𝔼 → (P : Tag 𝔼 → Set ℓ) → Set ℓ)
    (λ P → ⊤)
    (λ l 𝔼 ind P → P ze × ind (P ∘ su))

  case′ : ∀ {ℓ} (𝔼 : Enum) (P : Tag 𝔼 → Set ℓ)
    → (cs : Case′ 𝔼 P)
    → (t : Tag 𝔼) → P t
  case′ {ℓ} 𝔼 P cs t = elimTag
    (λ {𝔼} t → (P : Tag 𝔼 → Set ℓ) → Case′ 𝔼 P → P t)
    (λ P (c , cs) → c)
    (λ t ind P (c , cs) → ind (P ∘ su) cs)
    t
    P
    cs
