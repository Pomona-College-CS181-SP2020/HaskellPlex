-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
-- import Test.Tasty.Hspec
import Test.Tasty.HUnit

import ExplicitSimplexStream

-- EXAMPLE STREAMS --

stream1 :: Stream Int
stream1 = addSimplex (addVertex (addVertex (addVertex initializeStream 1) 2) 3) (Simplex [1,2])

stream2 :: Stream Int
stream2 = addVertex (addSimplex (addVertex (addVertex initializeStream 1) 2) (Simplex [2,1])) 3

stream3 :: Stream Int
stream3 = addVertex (addVertex (addVertex (addVertex initializeStream 1) 2) 3) 4

stream4 :: Stream Int
stream4 = initializeStream 

stream5 :: Stream Int
stream5 = addSimplex stream4 (Simplex [1,2,3,4])

----

main :: IO ()
main = do
  defaultMain (testGroup "SimplexStream tests" [addSingleVertexTest, initializeStreamTest, streamEqualityTest, streamNumVerticesEmptyTest, streamNumVertices4CellTest, streamGetSizeEmptyTest, streamGetSize4CellTest, streamGetSize4VertexTest, streamToOrderedSimplexListFourVerticesTest, streamToOrderedSimplexListThreeVerticesOneEdgeTest])


addSingleVertexTest :: TestTree
addSingleVertexTest = testCase "Testing addition of single vertex"
  (assertEqual "Should return true for search of vertex 5" (True) (isVertexInStream (addVertex (initializeStream :: Stream Int) 5) 5))

initializeStreamTest :: TestTree
initializeStreamTest = testCase "Testing initialization of stream"
  (assertEqual "Should return Simplices []" (Simplices [Simplex []] :: Stream Int) (initializeStream :: Stream Int))

-- Testing stream equality. 
-- Order of Simplex in Stream data type _should not matter_
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

-- Test streamToOrderedSimplexList 
streamToOrderedSimplexListFourVerticesTest :: TestTree
streamToOrderedSimplexListFourVerticesTest = testCase "Testing streamToOrderedSimplexList on stream with 4 vertices."
  (assertEqual "Should return object with lengths 0 and 1 with four vertices inside the 1 key." (OrderedSimplexList [SimplexListByDegree 0 [(Simplex [])], SimplexListByDegree 1 [(Simplex [1]), (Simplex [2]), (Simplex [3]), (Simplex [4])]]) (streamToOrderedSimplexList stream3))

streamToOrderedSimplexListThreeVerticesOneEdgeTest :: TestTree
streamToOrderedSimplexListThreeVerticesOneEdgeTest = testCase "Testing streamToOrderedSimplexList on stream with 3 vertices and one edge."
  (assertEqual "Should return object with lengths 0, 1, and 2 with three vertices inside the 1 key and 1 edge inside the 2 key." (OrderedSimplexList [SimplexListByDegree 0 [(Simplex [])], SimplexListByDegree 1 [(Simplex [1]), (Simplex [2]), (Simplex [3])], SimplexListByDegree 2 [(Simplex [1,2])]]) (streamToOrderedSimplexList stream1))