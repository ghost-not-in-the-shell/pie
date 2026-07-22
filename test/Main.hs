module Main where
import Data.Either
import Prelude hiding (curry, uncurry)
import Test.Tasty
import Test.Tasty.HUnit
import Raw  as R
import Core as T
import Parser (parseProg)
import Elab   (norm₀)

bracket_example ∷ (String , Maybe Raw , Tm)
bracket_example =
  ( "let x : Case {'a,'b,'c} (λ e ⇒ ℕ) = [1,2,3] in x"
  , Just $ R.Let "x" (R.Case (R.Brace [R.Tick "a",R.Tick "b",R.Tick "c"]) (R.lam "e" R.Nat)) (R.Bracket [R.nat 1, R.nat 2, R.nat 3]) (R.Var "x")
  , T.Pair (T.nat 1) (T.Pair (T.nat 2) (T.Pair (T.nat 3) T.Sole))
  )

switch_example ∷ (String , Maybe Raw , Tm)
switch_example =
  ( "let abc : Enum = {'a,'b,'c} in " ++
    "let f : Tag abc → ℕ = switch (λ t ⇒ ℕ) " ++
       "{ #a ⇒ 1 " ++
       "; #b ⇒ 2 " ++
       "; #c ⇒ 3 " ++
       "} in " ++
    "f"

  , Just $ R.Let "abc" R.Enum (R.Brace [R.Tick "a",R.Tick "b",R.Tick "c"])
  $ R.Let "f" (R.Pi "_" (R.Tag (R.Var "abc")) R.Nat)
      (R.Lam "@t" (Just (R.Tag (R.Brace [R.Tick "a",R.Tick "b",R.Tick "c"]))) (R.Switch (R.Var "@t") (R.lam "t" R.Nat)
       [("a" , R.nat 1)
       ,("b" , R.nat 2)
       ,("c" , R.nat 3)]))
  $ R.Var "f"

  , T.lam "t" $ T.Switch (T.FVar "t") (T.lam "t" T.Nat)
  $ T.Pair (T.nat 1) $ T.Pair (T.nat 2) $ T.Pair (T.nat 3) T.Sole
  )

identity ∷ (String , Maybe Raw , Tm)
identity =
  ( "let id : (A : Set) (x : A) → A = λ A x ⇒ x in id"
  , Just $ R.Let "id" (R.Pi "A" R.Set $ R.Pi "x" (R.Var "A") $ R.Var "A")
               (R.lam "A" $ R.lam "x" $ R.Var "x")
  $ R.Var "id"
  , T.lam "A" $ T.lam "x" $ T.FVar "x"
  )

curry ∷ (String , Maybe Raw , Tm)
curry =
  ( "let curry : (A : Set) (B : Set) (C : Set) → (A × B → C) → (A → B → C) " ++
              "= λ A B C f x y ⇒ f (x , y) " ++
    "in curry"

  , Just $ R.Let "curry" ( R.Pi "A" R.Set
                  $ R.Pi "B" R.Set
                  $ R.Pi "C" R.Set
                  $ R.Pi "_" (R.Pi "_" (R.Sg "_" (R.Var "A") (R.Var "B")) (R.Var "C"))
                  $ R.Pi "_" (R.Var "A") (R.Pi "_" (R.Var "B") (R.Var "C"))
                  )
                  ( R.lam "A" $ R.lam "B" $ R.lam "C" $ R.lam "f" $ R.lam "x" $ R.lam "y"
                  $ R.Var "f" `R.App` R.Pair (R.Var "x") (R.Var "y")
                  )
  $ R.Var "curry"

  , T.lam "A" $ T.lam "B" $ T.lam "C" $ T.lam "f" $ T.lam "x" $ T.lam "y"
  $ T.FVar "f" `T.App` T.Pair (T.FVar "x") (T.FVar "y")
  )

uncurry ∷ (String , Maybe Raw , Tm)
uncurry =
  ( "let uncurry : (A : Set) (B : Set) (C : Set) → (A → B → C) → A × B → C " ++
                "= λ A B C f p ⇒ f (fst p) (snd p) " ++
    "in uncurry"

  , Just $ R.Let "uncurry" ( R.Pi "A" R.Set
                    $ R.Pi "B" R.Set
                    $ R.Pi "C" R.Set
                    $ R.Pi "_" (R.Pi "_" (R.Var "A") (R.Pi "_" (R.Var "B") (R.Var "C")))
                    $ R.Pi "_" (R.Sg "_" (R.Var "A") (R.Var "B")) (R.Var "C")
                    )
                    ( R.lam "A" $ R.lam "B" $ R.lam "C" $ R.lam "f" $ R.lam "p"
                    $ R.Var "f" `R.App` R.Fst (R.Var "p") `R.App` R.Snd (R.Var "p")
                    )
  $ R.Var "uncurry"

  , T.lam "A" $ T.lam "B" $ T.lam "C" $ T.lam "f" $ T.lam "p"
  $ T.FVar "f" `T.App` T.Fst (T.FVar "p") `T.App` T.Snd (T.FVar "p")
  )

listE ∷ (String , Maybe Raw , Tm)
listE = ( "let ListE : Enum = {'Nil , 'Cons} in " ++
          "let Cons : Tag ListE = #Cons in " ++
          "Cons"

        , Just $ R.Let "ListE" R.Enum (R.Brace [R.Tick "Nil" , R.Tick "Cons"])
        $ R.Let "Cons" (R.Tag (R.Var "ListE")) (R.Sharp "Cons")
        $ R.Var "Cons"

        , T.Su (T.Tick "Nil") (T.Cons (T.Tick "Cons") T.Nil)
        $ T.Ze (T.Tick "Cons")  T.Nil
        )

append ∷ (String , Maybe Raw , Tm)
append =
  ( "let append : Enum → Enum → Enum " ++
               "= λ xs ys ⇒ elimEnum xs " ++
                  "(λ xs ⇒ Enum) " ++
                  "ys " ++
                  "(λ x xs append_xs_ys ⇒ cons x append_xs_ys) " ++
    "in append {'a , 'b} {'x , 'y}"

  , Just $ R.Let "append" (R.Pi "_" R.Enum (R.Pi "_" R.Enum R.Enum))
                   (R.lam "xs" $ R.lam "ys" $ R.ElimEnum (R.Var "xs")
                     (R.lam "xs" R.Enum)
                     (R.Var "ys")
                     (R.lam "x" $ R.lam "xs" $ R.lam "append_xs_ys" $
                      R.Cons (R.Var "x") (R.Var "append_xs_ys")))
  $ R.Var "append" `R.App` R.Brace [R.Tick "a",R.Tick "b"]
                   `R.App` R.Brace [R.Tick "x",R.Tick "y"]

  , T.Cons (T.Tick "a") (T.Cons (T.Tick "b") (T.Cons (T.Tick "x") (T.Cons (T.Tick "y") T.Nil)))
  )

toNat ∷ (String , Maybe Raw , Tm)
toNat =
  ( "let toNat : (e : Enum) → Tag e → ℕ " ++
              "= λ e t ⇒ elimTag t " ++
                 "(λ e t ⇒ ℕ) " ++
                 "(λ l e ⇒ zero) " ++
                 "(λ l e t toNat_t ⇒ suc toNat_t) in " ++
    "let abc : Enum = {'a , 'b , 'c} in " ++
    "let b : Tag abc = #b in " ++
    "toNat abc b"

  , Just $ R.Let "toNat" (R.Pi "e" R.Enum $ R.Pi "_" (R.Tag (R.Var "e")) R.Nat)
                  (R.lam "e" $ R.lam "t" $ R.ElimTag (R.Var "t")
                    (R.lam "e" $ R.lam "t" $ R.Nat)
                    (R.lam "l" $ R.lam "e" $ R.Zero)
                    (R.lam "l" $ R.lam "e" $ R.lam "t" $ R.lam "toNat_t" $ R.Suc (R.Var "toNat_t")))
  $ R.Let "abc" R.Enum (R.Brace [R.Tick "a",R.Tick "b", R.Tick "c"])
  $ R.Let "b" (R.Tag (R.Var "abc")) (R.Sharp "b")
  $ R.Var "toNat" `R.App` R.Var "abc" `R.App` R.Var "b"

  , T.Suc T.Zero
  )

bool ∷ (String , Maybe Raw , Tm)
bool =
  ( "let boolE  : Enum = {'true , 'false} in " ++
    "let trueD  : Desc = end in " ++
    "let falseD : Desc = end in " ++
    "let trueD_falseD : Tag boolE → Desc = " ++
      "switch (λ t ⇒ Desc) " ++
      "{ #true  ⇒ trueD " ++
      "; #false ⇒ falseD " ++
      "} in " ++
    "let boolD : Desc = arg (Tag boolE) trueD_falseD in " ++
    "let bool : Set = μ boolD in " ++
    "let trueT : ⟦ boolD ⟧ bool = [ #true ] in " ++
    "let true : bool = inj trueT in " ++
    "true"

  , Just $ R.Let "boolE"  R.Enum (R.Brace [R.Tick "true",R.Tick "false"])
  $ R.Let "trueD"  R.Desc R.End
  $ R.Let "falseD" R.Desc R.End
  $ R.Let "trueD_falseD" (R.Pi "_" (R.Tag (R.Var "boolE")) R.Desc)
    (R.Lam "@t" (Just $ R.Tag $ R.Brace [R.Tick "true",R.Tick "false"]) $ R.Switch (R.Var "@t") (R.lam "t" R.Desc)
     [("true" , R.Var "trueD")
     ,("false", R.Var "falseD")])
  $ R.Let "boolD" R.Desc (R.Arg (R.Tag (R.Var "boolE")) (R.Var "trueD_falseD"))
  $ R.Let "bool" R.Set (R.Mu (R.Var "boolD"))
  $ R.Let "trueT" (R.Lam "@X" (Just R.Set) (R.El (R.Var "boolD") (R.Var "@X")) `R.App` R.Var "bool") (R.Bracket [R.Sharp "true"])
  $ R.Let "true" (R.Var "bool") (R.Inj (R.Var "trueT"))
  $ R.Var "true"

  , T.Inj $ T.Ze (T.Tick "true") (T.Tick "false" `T.Cons` T.Nil) `T.Pair` T.Sole 
  )

list ∷ (String , Maybe Raw , Tm)
list =
  ( "let listE : Enum = {'Nil , 'Cons} in " ++
    "let nilD  : Desc = end in " ++
    "let consD : Set → Desc = λ A ⇒ arg A (λ a ⇒ rec end) in " ++
    "let nilD_consD : Set → Tag listE → Desc = λ A ⇒ " ++
      "switch (λ e ⇒ Desc) " ++
      "{ #Nil  ⇒ nilD " ++
      "; #Cons ⇒ consD A " ++
      "} in " ++
    "let listD : Set → Desc = λ A ⇒ " ++
      "arg (Tag listE) (nilD_consD A) in " ++
    "let list : Set → Set = λ A ⇒ μ (listD A) in " ++
    "let Cons : ℕ → list ℕ → list ℕ = λ x xs ⇒ inj [ #Cons , x , xs ] in " ++
    "let Nil : list ℕ = inj [ #Nil ] in " ++
    "Cons 0 Nil"

  , Nothing
  {-
  , R.Let "listE" R.Enum (R.Brace [R.Tick "Nil",R.Tick "Cons"])
  $ R.Let "nilD"  R.Desc R.End
  $ R.Let "consD" (R.Pi "_" R.Set R.Desc) (R.lam "A" $ R.Arg (R.Var "A") (R.lam "a" $ R.Rec R.End))
  $ R.Let "nilD_consD" (R.Pi "_" R.Set $ R.Pi "_" (R.Tag (R.Var "listE")) R.Desc)
      (R.lam "A"
      $ R.Lam "@t" (Just (R.Tag (R.Brace [R.Tick "Nil",R.Tick "Cons"]))) $ R.Switch (R.Var "@t") (R.lam "e" R.Desc)
      $ [("Nil" , R.Var "nilD")
        ,("Cons", R.Var "consD" `R.App` R.Var "A")])
  $ R.Let "listD" (R.Pi "_" R.Set R.Desc)
      (R.lam "A" $ R.Arg (R.Tag (R.Var "listE")) (R.Var "nilD_consD" `R.App` R.Var "A"))
  $ R.Let "list" (R.Pi "_" R.Set R.Set) (R.lam "A" $ R.Mu $ R.Var "listD" `R.App` R.Var "A")
  $ R.Let "ds" (R.Lam "@X" (Just R.Set) (R.El (R.Var "listD" `R.App` R.Nat) (R.Var "@X")) `R.App` (R.Var "list" `R.App` R.Nat)) (R.Bracket [R.Sharp "Nil"])
  $ R.Let "xs" (R.Var "list" `R.App` R.Nat) (R.Inj (R.Var "ds"))
  $ R.Var "list"
-}

  , T.Inj
  $ T.Su (T.Tick "Nil") (T.Cons (T.Tick "Cons") T.Nil) (T.Ze (T.Tick "Cons") T.Nil)
  `T.Pair`
  (T.Zero
   `T.Pair`
   (T.Inj (T.Ze (T.Tick "Nil") (T.Cons (T.Tick "Cons") T.Nil)
           `T.Pair` T.Sole)
     `T.Pair`
     T.Sole))
  )  

add ∷ (String , Maybe Raw , Tm)
add =
  ( "let NatE : Enum = {'zero_ , 'suc_} in " ++
    "let zeroD : Desc = end in " ++
    "let sucD : Desc = rec end in " ++
    "let NatC : Tag NatE → Desc = " ++
      "switch (λ t ⇒ Desc) " ++
      "{ #zero_ ⇒ zeroD " ++
      "; #suc_  ⇒ sucD } in " ++
    "let NatD : Desc = " ++
      "arg (Tag NatE) NatC in " ++
    "let Nat_ : Set = μ NatD in " ++
    "let zero_ : Nat_ = inj [ #zero_ ] in " ++
    "let suc_ : Nat_ → Nat_ = λ n ⇒ inj [ #suc_ , n ] in " ++
    "let curry : (A : Set) (B : A → Set) (C : (x : A) × B x → Set) → ((x : A) (y : B x) → C (x , y)) → ((p : (x : A) × B x) → C p) = " ++
      "λ A B C f p ⇒ f (fst p) (snd p) in " ++
    "let Toℕ : Nat_ → Set = λ n ⇒ ℕ in " ++
    "let zeroB : (xs : ⟦ zeroD ⟧ Nat_) (ihs : Hyps zeroD Nat_ Toℕ xs) → Toℕ (inj (#zero_ , xs)) = " ++
      "λ xs ihs ⇒ zero in " ++
    "let sucB : (xs : ⟦ sucD ⟧ Nat_) (ihs : Hyps sucD Nat_ Toℕ xs) → Toℕ (inj (#suc_ , xs)) = " ++
      "λ xs ihs ⇒ suc (fst ihs) in " ++

      {-
    "let toℕα : (xs : ⟦ NatD ⟧ Nat_) (ihs : Hyps NatD Nat_ Toℕ xs) → Toℕ (inj xs) = " ++
      "λ xs ⇒ switch (λ (t : Tag NatE) ⇒ (ds : ⟦ NatC t ⟧ Nat_) (ihs : Hyps NatD Nat_ Toℕ (t , ds)) → Toℕ (inj (t , ds))) " ++
      "{ #zero_ ⇒ zeroB " ++
      "; #suc_  ⇒ sucB } " ++
      "(fst xs) (snd xs) in " ++
-}
    "let toℕ : Nat_ → ℕ = λ n ⇒ elim n " ++
      "Toℕ" ++
      "(curry (Tag NatE) " ++
             "(λ t ⇒ ⟦ NatC t ⟧ Nat_) " ++
             "(λ t ⇒ Hyps NatD Nat_ Toℕ t → Toℕ (inj t)) " ++
         "(switch (λ t ⇒ (ds : ⟦ NatC t ⟧ Nat_) → Hyps NatD Nat_ Toℕ (t , ds) → Toℕ (inj (t , ds))) " ++
         "{ #zero_ ⇒ λ ds hs ⇒ zero " ++
         "; #suc_  ⇒ λ ds hs ⇒ suc (fst hs) } )) in " ++
    "let three : Nat_ = suc_ (suc_ (suc_ zero_)) in " ++
    "toℕ three" 

  , Nothing

  , T.Suc (T.Suc (T.Suc T.Zero)) )

examples ∷ [(String , Maybe Raw , Tm)]
examples = [ bracket_example, switch_example, identity, curry, uncurry, listE, append, toNat, bool, list, add ]

tests ∷ TestTree
tests = testGroup "Pie" $
  map (\ (src , raw , tm) →
         let stage₁ = parseProg src in
         testGroup src $
           case raw of
             Just raw →
               (testCase "Parser" $ stage₁ @?= Right raw)
               : if isRight stage₁
                 then let stage₂ = norm₀ (fromRight undefined stage₁) in
                  [ testCase "Elab" $ stage₂ @?= Right tm ]
                 else []
             Nothing →
               (testCase "Parser" $ isRight stage₁ @? show stage₁)
               : if isRight stage₁
                 then let stage₂ = norm₀ (fromRight undefined stage₁) in
                  [ testCase "Elab" $ stage₂ @?= Right tm ]
                 else [])
      examples

main ∷ IO ()
main = defaultMain tests
