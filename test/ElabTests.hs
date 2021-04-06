module ElabTests where
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Text.ParserCombinators.Parsec
import Parser
import Elab

church_encoding =
     "let Nat  : Set = ∀ (N : Set) (_ : ∀ (_ : N) → N) (_ : N) → N in "
  ++ "let five : Nat = λ N s z → s (s (s (s (s z)))) in "
  ++ "let add  : ∀ (_ : Nat) (_ : Nat) → Nat = λ a b N s z → a N s (b N s z) in "
  ++ "add five five"


elabTests ∷ TestTree
elabTests = testGroup "Elab"
  [ testCase "church encoding" $
      let raw = fromRight undefined $ parse prog "<stdin>" church_encoding in
      isRight (runElab₀ (infer raw)) @? "error"
  ]
