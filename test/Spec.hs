import Test.Tasty
import ParserTests
import ElabTests

allTests ∷ TestTree
allTests = testGroup "Pie"
  [ parserTests
  , elabTests
  ]

main ∷ IO ()
main = defaultMain allTests
