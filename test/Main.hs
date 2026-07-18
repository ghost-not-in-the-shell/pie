module Main where
import Data.Either
import Prelude hiding (curry, uncurry)
import Test.Tasty
import Test.Tasty.HUnit
import Raw  as R
import Core as T
import Parser (parseProg)
import Elab   (norm₀)

identity ∷ (String , Raw , Tm)
identity = ( "let id : (A : Set) (x : A) → A = λ A x ⇒ x in id"
           , R.Let "id" (R.Pi "A" R.Set $ R.Pi "x" (R.Var "A") $ R.Var "A")
                        (R.lam "A" $ R.lam "x" $ R.Var "x")
           $ R.Var "id"
           , T.lam "A" $ T.lam "x" $ T.FVar "x"
           )

curry ∷ (String , Raw , Tm)
curry = ( "let curry : (A : Set) (B : Set) (C : Set) → (A × B → C) → (A → B → C) " ++
                    "= λ A B C f x y ⇒ f (x , y) " ++
          "in curry"
        , R.Let "curry" ( R.Pi "A" R.Set
                        $ R.Pi "B" R.Set
                        $ R.Pi "C" R.Set
                        $ R.Pi "_" (R.Pi "_" (R.Sg "_" (R.Var "A") (R.Var "B")) (R.Var "C"))
                        $ R.Pi "_" (R.Var "A") (R.Pi "_" (R.Var "B") (R.Var "C"))
                        )
                        ( R.lam "A" $ R.lam "B" $ R.lam "C" $ R.lam "f" $ R.lam "x" $ R.lam "y"
                        $ R.App (R.Var "f") (R.Pair (R.Var "x") (R.Var "y"))
                        )
        $ R.Var "curry"
        , T.lam "A" $ T.lam "B" $ T.lam "C" $ T.lam "f" $ T.lam "x" $ T.lam "y"
        $ T.App (T.FVar "f") (T.Pair (T.FVar "x") (T.FVar "y"))
        )

uncurry ∷ (String , Raw , Tm)
uncurry = ( "let uncurry : (A : Set) (B : Set) (C : Set) → (A → B → C) → A × B → C " ++
                        "= λ A B C f p ⇒ f (fst p) (snd p) " ++
            "in uncurry"
          , R.Let "uncurry" ( R.Pi "A" R.Set
                            $ R.Pi "B" R.Set
                            $ R.Pi "C" R.Set
                            $ R.Pi "_" (R.Pi "_" (R.Var "A") (R.Pi "_" (R.Var "B") (R.Var "C")))
                            $ R.Pi "_" (R.Sg "_" (R.Var "A") (R.Var "B")) (R.Var "C")
                            )
                            ( R.lam "A" $ R.lam "B" $ R.lam "C" $ R.lam "f" $ R.lam "p"
                            $ R.App (R.App (R.Var "f") (R.Fst (R.Var "p"))) (R.Snd (R.Var "p"))
                            )
          $ R.Var "uncurry"
          , T.lam "A" $ T.lam "B" $ T.lam "C" $ T.lam "f" $ T.lam "p"
          $ T.App (T.App (T.FVar "f") (T.Fst (T.FVar "p")))
                  (T.Snd (T.FVar "p"))
          )

listE ∷ (String , Raw , Tm)
listE = ( "let ListE : Enum = {'nil , 'cons} in ListE"
        , R.Let "ListE" R.Enum (R.Brace [R.Tick "nil" , R.Tick "cons"]) $ R.Var "ListE"
        , T.Cons (T.Tick "nil") $ T.Cons (T.Tick "cons") T.Nil
        )

examples ∷ [(String , Raw , Tm)]
examples = [identity,curry,uncurry,listE]

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
