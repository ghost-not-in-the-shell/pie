module Desc.Prelude where
open import Agda.Builtin.Bool     public
open import Agda.Builtin.Equality public
open import Agda.Builtin.List     public
open import Agda.Builtin.Nat      public renaming (Nat to ℕ)
open import Agda.Builtin.Sigma    public
open import Agda.Builtin.String   public
open import Agda.Primitive        public

record ⊤ {ℓ} : Set ℓ where
  constructor tt

cong : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂}
  → (f : A → B)
  → ∀ {x y}
  → x ≡ y
  → f x ≡ f y
cong f refl = refl

infix 2 Σ-syntax
Σ-syntax = Σ
syntax Σ-syntax A (λ x → B) = [ x ∈ A ] × B

infixr 2 _×_
_×_ : ∀ {ℓ₁ ℓ₂} → Set ℓ₁ → Set ℓ₂ → Set (ℓ₁ ⊔ ℓ₂)
A × B = Σ A λ _ → B

data Fin : ℕ → Set where
  zero : ∀ {n}         → Fin (suc n)
  suc  : ∀ {n} → Fin n → Fin (suc n)

length : ∀ {ℓ} {A : Set ℓ} → List A → ℕ
length [] = 0
length (x ∷ xs) = suc (length xs)

lookup : ∀ {ℓ} {A : Set ℓ} (xs : List A) → Fin (length xs) → A
lookup [] ()
lookup (x ∷ xs) zero    = x
lookup (x ∷ xs) (suc i) = lookup xs i

const : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂}
  → A → B → A
const x = λ _ → x

flip : ∀ {ℓ₁ ℓ₂ ℓ₃} {A : Set ℓ₁} {B : Set ℓ₂} {C : A → B → Set ℓ₃}
  → (∀ x y → C x y) → (∀ y x → C x y)
flip f = λ y x → f x y

infixr -1 _$_
_$_ : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : A → Set ℓ₂}
  → (∀ x → B x) → (∀ x → B x)
f $ x = f x

id : ∀ {ℓ} {A : Set ℓ} → A → A
id = λ x → x

infixr 5 _∘_
_∘_ : ∀ {ℓ₁ ℓ₂ ℓ₃}
  → {A : Set ℓ₁}
  → {B : A → Set ℓ₂}
  → {C : (x : A) (y : B x) → Set ℓ₃}
  → (g : ∀ {x} y → C x y)
  → (f : ∀ x → B x)
  → ∀ x → C x (f x)
g ∘ f = λ x → g (f x)

curry : ∀ {ℓ₁ ℓ₂ ℓ₃}
  → {A : Set ℓ₁} {B : A → Set ℓ₂} {P : Σ A B → Set ℓ₃}
  → ((x : A) (y : B x) → P (x , y))
  → ((p : Σ A B) → P p)
curry f = λ (x , y) → f x y
  
_∋_ : ∀ {ℓ} (A : Set ℓ) → A → A
A ∋ a = a
