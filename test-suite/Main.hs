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
  defaultMain (testGroup "SimplexStream tests" [addSingleVertexTest, initializeStreamTest, streamEqualityTest])

addSingleVertexTest :: TestTree
addSingleVertexTest = testCase "Testing addition of single vertex"
  (assertEqual "Should return true for search of vertex 5" True (isVertexInStream (addVertex initializeStream 5) 5))

initializeStreamTest :: TestTree
initializeStreamTest = testCase "Testing initialization of stream"
  (assertEqual "Should return Simplicies []" (Simplicies [Simplex []]) (initializeStream))

-- FAILING TEST
stream1 :: Stream 
stream1 = addEdge (addVertex (addVertex (addVertex initializeStream 1) 2) 3) 1 2

stream2 :: Stream 
stream2 = addVertex (addEdge (addVertex (addVertex initializeStream 1) 2) 2 1) 3

streamEqualityTest :: TestTree 
streamEqualityTest = testCase "Testing quality of streams"
  (assertEqual "Should return Simplicies []" (True) (stream1 == stream2))