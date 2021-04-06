module ParserTests where
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec
import Raw
import Parser

success ∷ [(String, Raw)]
success = [ ( "λ x → x"
            , Lam "x" $ Var "x" )

          , ( "∀ (A : Set) (x : A) → A"
            , Pi "A" Set $ Pi "x" (Var "A") $ Var "A" )

          , ( "let id : ∀ (A : Set) (x : A) → A = λ x → x in id"
            , Let "id" (Pi "A" Set $ Pi "x" (Var "A") $ Var "A")
                       (Lam "x" $ Var "x") $
              Var "id" )
          , ( "x y z"
            , App (App (Var "x") (Var "y")) (Var "z") )

          , ( "(λ x → x) x"
            , App (Lam "x" $ Var "x") (Var "x") )

          , ( "λ x y z → x"
            , Lam "x" $ Lam "y" $ Lam "z" $ Var "x" )

          , ( "car (cons x y)"
            , Car $ Cons (Var "x") (Var "y") )
          ]

failure ∷ [String]
failure = [ "λ x y →"
          , "cons x"
          ]

parserTests ∷ TestTree
parserTests = testGroup "Parser"
  [ testGroup "Success" $
      map (\ (input, output) →
             testCase input $ parse prog "<stdin>" input @?= Right output)
          success
  , testGroup "Failure" $
      map (\ input →
             let output = parse prog "stdin" input in
             testCase input $ isLeft output @? show output)
          failure
  ]
