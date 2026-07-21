module Main where
import Data.Either
import Prelude hiding (curry, uncurry)
import Test.Tasty
import Test.Tasty.HUnit
import Raw  as R
import Core as T
import Parser (parseProg)
import Elab   (norm₀)

bracket_example ∷ (String , Raw , Tm)
bracket_example =
  ( "let x : Case {'a,'b,'c} (λ e ⇒ ℕ) = [1,2,3] in x"
  , R.Let "x" (R.Case (R.Brace [R.Tick "a",R.Tick "b",R.Tick "c"]) (R.lam "e" R.Nat)) (R.Bracket [R.nat 1, R.nat 2, R.nat 3]) (R.Var "x")
  , T.Pair (T.nat 1) (T.Pair (T.nat 2) (T.Pair (T.nat 3) T.Sole))
  )

switch_example ∷ (String , Raw , Tm)
switch_example =
  ( "let abc : Enum = {'a,'b,'c} in " ++
    "let f : Tag abc → ℕ = switch (λ t ⇒ ℕ) " ++
       "{ #a ⇒ 1 " ++
       "; #b ⇒ 2 " ++
       "; #c ⇒ 3 " ++
       "} in " ++
    "f"

  , R.Let "abc" R.Enum (R.Brace [R.Tick "a",R.Tick "b",R.Tick "c"])
  $ R.Let "f" (R.Pi "_" (R.Tag (R.Var "abc")) R.Nat)
      (R.Lam "@t" (Just (R.Tag (R.Brace [R.Tick "a",R.Tick "b",R.Tick "c"]))) (R.Switch (R.Var "@t") (R.lam "t" R.Nat)
       [("a" , R.nat 1)
       ,("b" , R.nat 2)
       ,("c" , R.nat 3)]))
  $ R.Var "f"

  , T.lam "t" $ T.Switch (T.FVar "t") (T.lam "t" T.Nat)
  $ T.Pair (T.nat 1) $ T.Pair (T.nat 2) $ T.Pair (T.nat 3) T.Sole
  )

identity ∷ (String , Raw , Tm)
identity =
  ( "let id : (A : Set) (x : A) → A = λ A x ⇒ x in id"
  , R.Let "id" (R.Pi "A" R.Set $ R.Pi "x" (R.Var "A") $ R.Var "A")
               (R.lam "A" $ R.lam "x" $ R.Var "x")
  $ R.Var "id"
  , T.lam "A" $ T.lam "x" $ T.FVar "x"
  )

curry ∷ (String , Raw , Tm)
curry =
  ( "let curry : (A : Set) (B : Set) (C : Set) → (A × B → C) → (A → B → C) " ++
              "= λ A B C f x y ⇒ f (x , y) " ++
    "in curry"

  , R.Let "curry" ( R.Pi "A" R.Set
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

uncurry ∷ (String , Raw , Tm)
uncurry =
  ( "let uncurry : (A : Set) (B : Set) (C : Set) → (A → B → C) → A × B → C " ++
                "= λ A B C f p ⇒ f (fst p) (snd p) " ++
    "in uncurry"

  , R.Let "uncurry" ( R.Pi "A" R.Set
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

listE ∷ (String , Raw , Tm)
listE = ( "let ListE : Enum = {'Nil , 'Cons} in " ++
          "let Cons : Tag ListE = #Cons in " ++
          "Cons"

        , R.Let "ListE" R.Enum (R.Brace [R.Tick "Nil" , R.Tick "Cons"])
        $ R.Let "Cons" (R.Tag (R.Var "ListE")) (R.Sharp "Cons")
        $ R.Var "Cons"

        , T.Su (T.Tick "Nil") (T.Cons (T.Tick "Cons") T.Nil)
        $ T.Ze (T.Tick "Cons")  T.Nil
        )

append ∷ (String , Raw , Tm)
append =
  ( "let append : Enum → Enum → Enum " ++
               "= λ xs ys ⇒ elimEnum xs " ++
                  "(λ xs ⇒ Enum) " ++
                  "ys " ++
                  "(λ x xs append_xs_ys ⇒ cons x append_xs_ys) " ++
    "in append {'a , 'b} {'x , 'y}"

  , R.Let "append" (R.Pi "_" R.Enum (R.Pi "_" R.Enum R.Enum))
                   (R.lam "xs" $ R.lam "ys" $ R.ElimEnum (R.Var "xs")
                     (R.lam "xs" R.Enum)
                     (R.Var "ys")
                     (R.lam "x" $ R.lam "xs" $ R.lam "append_xs_ys" $
                      R.Cons (R.Var "x") (R.Var "append_xs_ys")))
  $ R.Var "append" `R.App` R.Brace [R.Tick "a",R.Tick "b"]
                   `R.App` R.Brace [R.Tick "x",R.Tick "y"]

  , T.Cons (T.Tick "a") (T.Cons (T.Tick "b") (T.Cons (T.Tick "x") (T.Cons (T.Tick "y") T.Nil)))
  )

toNat ∷ (String , Raw , Tm)
toNat =
  ( "let toNat : (e : Enum) → Tag e → Nat " ++
              "= λ e t ⇒ elimTag t " ++
                 "(λ e t ⇒ Nat) " ++
                 "(λ l e ⇒ zero) " ++
                 "(λ l e t toNat_t ⇒ suc toNat_t) in " ++
    "let abc : Enum = {'a , 'b , 'c} in " ++
    "let b : Tag abc = #b in " ++
    "toNat abc b"

  , R.Let "toNat" (R.Pi "e" R.Enum $ R.Pi "_" (R.Tag (R.Var "e")) R.Nat)
                  (R.lam "e" $ R.lam "t" $ R.ElimTag (R.Var "t")
                    (R.lam "e" $ R.lam "t" $ R.Nat)
                    (R.lam "l" $ R.lam "e" $ R.Zero)
                    (R.lam "l" $ R.lam "e" $ R.lam "t" $ R.lam "toNat_t" $ R.Suc (R.Var "toNat_t")))
  $ R.Let "abc" R.Enum (R.Brace [R.Tick "a",R.Tick "b", R.Tick "c"])
  $ R.Let "b" (R.Tag (R.Var "abc")) (R.Sharp "b")
  $ R.Var "toNat" `R.App` R.Var "abc" `R.App` R.Var "b"

  , T.Suc T.Zero
  )

boolD ∷ (String , Raw , Tm)
boolD =
  ( "let boolE  : Enum = {'true , 'false} in " ++
    "let trueD  : Desc = end in " ++
    "let falseD : Desc = end in " ++
    "let trueD_falseD : Tag boolE → Desc = " ++
      "switch (λ t ⇒ Desc) " ++
      "{ #true  ⇒ trueD " ++
      "; #false ⇒ falseD " ++
      "} in " ++
    "let boolD : Desc = arg (Tag boolE) trueD_falseD in " ++
    "boolD"

  , R.Let "boolE"  R.Enum (R.Brace [R.Tick "true",R.Tick "false"])
  $ R.Let "trueD"  R.Desc R.End
  $ R.Let "falseD" R.Desc R.End
  $ R.Let "trueD_falseD" (R.Pi "_" (R.Tag (R.Var "boolE")) R.Desc)
    (R.Lam "@t" (Just $ R.Tag $ R.Brace [R.Tick "true",R.Tick "false"]) $ R.Switch (R.Var "@t") (R.lam "t" R.Desc)
     [("true" , R.Var "trueD")
     ,("false", R.Var "falseD")])
  $ R.Let "boolD" R.Desc (R.Arg (R.Tag (R.Var "boolE")) (R.Var "trueD_falseD"))
  $ R.Var "boolD"

  , T.Arg (T.Tag (T.Cons (T.Tick "true") (T.Cons (T.Tick "false") T.Nil)))
  $ T.lam "t" $ T.Switch (T.FVar "t") (T.lam "t" T.Desc)
  $ T.End `T.Pair` (T.End `T.Pair` T.Sole)
  )

listD ∷ (String , Raw , Tm)
listD =
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
    "listD"

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
  $ R.Var "listD"

  , T.lam "A" $ T.Arg (T.Tag (T.Cons (T.Tick "Nil") (T.Cons (T.Tick "Cons") T.Nil)))
  $ T.lam "t" $ T.Switch (FVar "t") (T.lam "t" T.Desc)
  $ T.End `T.Pair` (T.Arg (FVar "A") (T.lam "_" (T.Rec T.End)) `T.Pair` T.Sole)
  )

examples ∷ [(String , Raw , Tm)]
examples = [ bracket_example, switch_example, identity, curry, uncurry, listE, append, toNat, boolD, listD ]

tests ∷ TestTree
tests = testGroup "Pie" $
  map (\ (src , raw , tm) →
         let stage₁ = parseProg src in
         testGroup src $
           (testCase "Parser" $ stage₁ @?= Right raw)
           : if isRight stage₁
             then let stage₂ = norm₀ (fromRight undefined stage₁) in
                  [ testCase "Elab" $ stage₂ @?= Right tm ]
             else [])
      examples

main ∷ IO ()
main = defaultMain tests
