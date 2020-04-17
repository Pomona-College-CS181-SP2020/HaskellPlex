-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
-- import Test.Tasty.Hspec
import Test.Tasty.HUnit

import SimplexStream

-- EXAMPLE STREAMS --

stream1 :: Stream 
stream1 = addSimplex (addVertex (addVertex (addVertex initializeStream 1) 2) 3) (Simplex [1,2])

stream2 :: Stream 
stream2 = addVertex (addSimplex (addVertex (addVertex initializeStream 1) 2) (Simplex [2,1])) 3

stream3 :: Stream 
stream3 = addVertex (addVertex (addVertex (addVertex initializeStream 1) 2) 3) 4

stream4 :: Stream 
stream4 = initializeStream 

stream5 :: Stream 
stream5 = addSimplex stream4 (Simplex [1,2,3,4])

----

main :: IO ()
main = do
  defaultMain (testGroup "SimplexStream tests" [addSingleVertexTest, initializeStreamTest, streamEqualityTest, streamNumVerticesEmptyTest, streamNumVertices4CellTest, streamGetSizeEmptyTest, streamGetSize4CellTest, streamGetSize4VertexTest])


addSingleVertexTest :: TestTree
addSingleVertexTest = testCase "Testing addition of single vertex"
  (assertEqual "Should return true for search of vertex 5" True (isVertexInStream (addVertex initializeStream 5) 5))

initializeStreamTest :: TestTree
initializeStreamTest = testCase "Testing initialization of stream"
  (assertEqual "Should return Simplices []" (Simplices [Simplex []]) (initializeStream))

-- Testing stream equality. Order of Simplex in Stream data type _should not matter_
streamEqualityTest :: TestTree 
streamEqualityTest = testCase "Testing quality of streams"
  (assertEqual "Should return Simplices []" (True) (stream1 == stream2))

-- Test numVertices
streamNumVerticesEmptyTest :: TestTree 
streamNumVerticesEmptyTest = testCase "Testing number of vertices in empty stream"
  (assertEqual "Should return 0" (0) (numVertices stream4))

streamNumVertices4CellTest :: TestTree 
streamNumVertices4CellTest = testCase "Testing number of vertices in a 4-cell"
  (assertEqual "Should return 4" (4) (numVertices stream5))

-- Test getSize 
streamGetSizeEmptyTest :: TestTree
streamGetSizeEmptyTest = testCase "Testing get size on empty stream."
  (assertEqual "Should return 1 (the null-cell)" (1) (getSize stream4))

streamGetSize4CellTest :: TestTree
streamGetSize4CellTest = testCase "Testing get size on 4-cell stream."
  (assertEqual "Should return 16" (16) (getSize stream5))

streamGetSize4VertexTest :: TestTree
streamGetSize4VertexTest = testCase "Testing get size on stream with 4 vertices."
  (assertEqual "Should return 5 (four vertices + 1 null cell)." (5) (getSize stream3))


