-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
-- import Test.Tasty.Hspec
import Test.Tasty.HUnit

import SimplexStream

main :: IO ()
main = do
  defaultMain (testGroup "SimplexStream tests" [addSingleVertexTest, initializeStreamTest])

addSingleVertexTest :: TestTree
addSingleVertexTest = testCase "Testing addition of single vertex"
  (assertEqual "Should return true for search of vertex 5" True (isVertexInStream (addVertex initializeStream 5) 5))

initializeStreamTest :: TestTree
initializeStreamTest = testCase "Testing initialization of stream"
  (assertEqual "Should return Simplicies []" (Simplicies []) (initializeStream))