-- | authors: Connor Ford, Jake Hauser

module ExplicitSimplexStream where

import Data.List (sort, subsequences, delete, intercalate, elemIndex)
import Data.Maybe (fromJust)

-- data type: Simplex [a]
-- A simplex is given by a list of n points representing a geometrical construction of dimension n-1.
-- e.g. Simplex [1,2,3] or Simplex ["cat", "dog", "cow"]
-- 
-- Alternative approach:
-- data Simplex = Simplex Int [a]
-- where the first term is the length (order) of the simplex.
---- advantages: this allows for much faster lookup. i.e. when iterating over a list of simplex we would not have to compute lengths.
---- disadvantages: more complexity for storage.
data Simplex a = Simplex [a]

instance (Show a) => Show (Simplex a) where 
    show (Simplex xs) = show xs

-- Equality instance for Simplex a
-- Algorithm:
-- (1) Sort simplex lists.
-- (2) Iterate over both lists simultaneously comparing elements.
-- note: we want Simplex [1,2,3] to equal Simplex [2,1,3]
-- note: requires that elements are order-able.
-- complexity: O(n log(n) + m log (m))
instance (Ord a) => Eq (Simplex a) where
    (Simplex []) == (Simplex []) = True
    (Simplex []) == (Simplex _)  = False 
    (Simplex _)  == (Simplex []) = False
    (Simplex xs) == (Simplex ys)
            | x /= y    = False 
            | otherwise = (Simplex rest_x) == (Simplex rest_y)
        where 
            (x:rest_x) = sort xs
            (y:rest_y) = sort ys

-- Given a simplex return a list of all subcomplexes.
-- e.g. for the simplex [1,2], the subcomplexes are [1], [2], and [].
-- note: this includes the trivial subcomplexes [] as well as the input simplex.
getSubcomplexes :: Simplex a -> [Simplex a]
getSubcomplexes (Simplex s) = map (\x -> (Simplex x)) (subsequences s)

-- given a simplex, return True if it is a vertex.
isVertex :: Simplex a  -> Bool
isVertex (Simplex [_x]) = True 
isVertex _              = False

-- get list of points from simplex
simplexLift :: Simplex a -> [a]
simplexLift (Simplex x) = x 

-- get value from vertex
vertexLift :: Simplex a -> a
vertexLift (Simplex [x]) = x 
vertexLift _             = error "the vertexLift method only takes vertices as input."

-- removeSimplexElement takes a simplex and the index n of some point in the simplex.
-- returns a list of points minus the point at index n (maintains original order).
removeSimplexElement :: Simplex a -> Int -> [a]
removeSimplexElement (Simplex [])     _ = []
removeSimplexElement (Simplex (x:xs)) n = 
    if n == 0 then 
        xs 
    else
        x:(removeSimplexElement (Simplex xs) (n-1))

-- isSimplexInSimplexList
-- Given a simplex and a list of simplexes, return True if the simplex is in the list. False, otherwise.
isSimplexInSimplexList :: (Ord a) => Simplex a -> [Simplex a] -> Bool 
isSimplexInSimplexList _ []     = False 
isSimplexInSimplexList s (x:xs) = 
    if x == s then 
        True 
    else 
        (isSimplexInSimplexList s xs) || False

-- data type: Stream a
-- A Stream is a non-trivial list of simplices.
-- note: If the list is empty then the stream is non-initialized. If this is the case
-- then any operations on the stream throw an error.
-- note: An equivalent concept is the simplicial complex. A simplicial complex is a non-empty set of finite sets closed under subsets. We require that these simplex streams satisfy this same property.
-- We use a stream rather than [Simplex a] when we want to connote that the list is (a) non-empty and (b) a simplicial complex.
data Stream a = Simplices [Simplex a]

instance (Show a) => Show (Stream a) where
    show (Simplices []) = error "Cannot display a non-initialized Stream."
    show (Simplices xs) = intercalate ", " (map show xs)

-- simplexInStream
-- Iterates over the stream and checks for equality with the given simplex.
-- complexity: O(n)
simplexInStream :: (Ord a) => Simplex a -> Stream a -> Bool 
simplexInStream simplex stream = 
    case stream of
        Simplices [] -> error "Cannot find simplex in a non-initialized stream."
        Simplices [x] -> simplex == x -- base case
        Simplices (x:xs) -> 
            if simplex == x then 
                True 
            else 
                simplexInStream simplex (Simplices xs)

-- isSubcomplex 
-- Given two streams, returns True if first stream is a subcomplex of the second stream.
isSubcomplex :: (Ord a) => Stream a -> Stream a -> Bool
isSubcomplex (Simplices [])     _       = error "Cannot find subcomplex of a non-initialized stream."
isSubcomplex (Simplices [x])    stream2 = simplexInStream x stream2 -- base case
isSubcomplex (Simplices (x:xs)) stream2 = 
    if not (simplexInStream x stream2) then 
        False 
    else 
        isSubcomplex (Simplices xs) stream2

-- Equality instance for Stream a
-- Two streams are equivalent if they contain identical simplices.
-- Algorithm: Two simplicial complexes are equal if both are subcomplexes of the other.
    -- i.e. For Streams X and Y. X == Y iff for all x in X, x in Y AND for all y in Y, y in X.
-- note: could implement more efficiently by looking only at highest-order simplices
instance (Ord a) => Eq (Stream a) where
    stream1 == stream2 = isSubcomplex stream1 stream2 && isSubcomplex stream2 stream1

-- initializeStream
-- Initializes a stream by creating a list with only the null cell.
initializeStream :: Stream a 
initializeStream = Simplices [(Simplex [])] -- initialize with null cell

-- addVertex
-- takes a stream of type a and a value, x, of type a,
-- return the same stream with a new value of Simplex [x] prepended to the front.
-- note: default filtration value is 0.
addVertex :: Stream a -> a -> Stream a
addVertex (Simplices xs) x = Simplices $ (Simplex [x]):xs

-- isVertexInStream
-- Given a stream of type a and value of type a,
-- Returns True if the value corresponding to a vertex is in the stream. Otherwise, return False.
isVertexInStream :: (Ord a) => Stream a -> a -> Bool
isVertexInStream (Simplices [])     _ = error "Cannot find vertex in a non-initialized stream."
isVertexInStream (Simplices [x])    v = (vertexLift x) == v
isVertexInStream (Simplices (x:xs)) v = 
    case x of
        Simplex [z] -> 
            if z == v then 
                True 
            else 
                isVertexInStream (Simplices xs) v
        Simplex _   -> isVertexInStream (Simplices xs) v

-- addSimplex 
-- Given a stream and a simplex, add the simplex and all subcomplexes to the stream if not already present.
-- requirement: all names in simplex list must be unique.
-- note: default filtration value 0.
addSimplex :: (Ord a) => Stream a -> Simplex a -> Stream a
addSimplex (Simplices []) _ = error "Cannot add a simplex to a non-initialized stream."
addSimplex (Simplices simps) simplex = 
    Simplices (foldl conditionallyAddSimplexToListOfSimplices simps subcomplexes)
    where 
        subcomplexes = getSubcomplexes simplex
        conditionallyAddSimplexToListOfSimplices = 
            \simplicesAccumulator subcomplex -> 
                if simplexInStream subcomplex (Simplices simplicesAccumulator) then 
                    simplicesAccumulator 
                else 
                    subcomplex:simplicesAccumulator

-- getSimplicesSizeN
-- Given a stream of simplices and some integer n,
-- return a list of simplices in the stream with size n.
getSimplicesSizeN :: (Ord a) => Stream a -> Int -> [Simplex a]
getSimplicesSizeN (Simplices [])     _ = error "Cannot get simplices for uninitialized stream."
getSimplicesSizeN (Simplices (x:xs)) n = 
    if length (simplexLift x) == n then 
        x:(getSimplicesSizeN (Simplices (xs)) n) 
    else 
        getSimplicesSizeN (Simplices (xs)) n

-- isMaxSimplex
-- Given a stream and a simplex, 
-- determines if the simplex is not a sub-simplex of any other simplices in the stream.
-- If the simplex is not a sub-simplex then it must be a max simplex.
-- algorithm: 
-- (1) get vertices.
-- (2) iterate over vertices, adding them to simplex.
-- (3) compare with simplices of length simplex + 1.
-- note: by default the empty simplex is never a max simplex 
-- even if it is the only simplex in the stream.
isMaxSimplex :: (Ord a) => Stream a -> Simplex a -> Bool
isMaxSimplex _      (Simplex []) = False -- base case / trivial case. 
isMaxSimplex stream (Simplex xs) = 
    let 
        len = length xs 
        stream_vertices = getSimplicesSizeN stream 1
        int_vertices = map vertexLift stream_vertices
        nPlusOneSimplices = getSimplicesSizeN stream (len + 1)
    in 
        foldl (\acc x -> if isSimplexInSimplexList (Simplex (x:xs)) nPlusOneSimplices then False else acc) True int_vertices

-- subtractSimplex
-- Given a stream and a simplex, if the simplex is a max simplex then remove it from the stream.
-- When subtracting a simplex from a simplicial complex, 
-- we must make sure that we maintain a valid simplicial complex. 
-- As a result, we can only remove a simplex if it is not a sub-simplex of
-- any other simplex in the stream.
subtractSimplex :: (Ord a) => Stream a -> Simplex a -> Stream a 
subtractSimplex (Simplices []) _ = error "Cannot subtract from a non-initialized stream."
subtractSimplex (Simplices xs) s = 
    if isMaxSimplex (Simplices xs) s then
        Simplices (delete s xs)
    else 
        Simplices xs

-- getSize
-- Given a stream, return the number of simplices in stream.
getSize :: Stream a -> Int
getSize (Simplices []) = error "Cannot get size of a non-initialized stream."
getSize (Simplices xs) = length xs

-- numVertices
-- Given a stream, return the number of vertices (1-cells) in the stream.
numVertices :: Stream a -> Int 
numVertices (Simplices []) = error "Cannot get the number of vertices for a non-initialized stream."
numVertices (Simplices xs) = foldl (\acc x -> if (isVertex x) then acc + 1 else acc) 0 xs

-- getVertices 
-- Given a stream of type a return a list of vertex values of type a.
getVertices :: Stream a -> [a]
getVertices (Simplices l) = 
    foldl(\acc simplex -> 
        if isVertex simplex then 
            (vertexLift simplex):acc 
        else 
            acc
    ) [] l

-- SimplexListByDegree
-- This data type represents a collection of simplices of identical length.
-- The parameters of SimplexListByDegree are an Int and a list of simplices.
-- Each simplex in the list must be the same length, specified by the first parameter.
data SimplexListByDegree a = SimplexListByDegree Int [Simplex a] deriving (Show)

-- Equality instance for SimplexListByDegree of type a
-- is given by equality of simplex lengths and simplex elements.
-- We can check for equality of simplex lists by casting the lists to Streams.
instance (Ord a) => Eq (SimplexListByDegree a) where 
    (SimplexListByDegree a xs) == (SimplexListByDegree b ys) = (a == b) && (Simplices xs == Simplices ys)

-- degreeOfSimplexListByDegree
-- given a SimplexListByDegree return the degree.
degreeOfSimplexListByDegree :: SimplexListByDegree a -> Int 
degreeOfSimplexListByDegree (SimplexListByDegree x _) = x

-- addSimplexToSimplexListByDegree
-- Given a simplex and a SimplexListByDegree, if the length of the simplex
-- matches the degree of the SimplexListByDegree, then append the simplex to the list.
addSimplexToSimplexListByDegree :: Simplex a -> SimplexListByDegree a -> SimplexListByDegree a
addSimplexToSimplexListByDegree simplex@(Simplex ys) (SimplexListByDegree degree xs) = 
    if length ys == degree then 
        SimplexListByDegree degree (simplex:xs)
    else 
        error "The length of the simplex does not match the degree of the SimplexListByDegree in the function addSimplexToSimplexListByDegree."

-- indexOfSimplex
-- Given a Simplex and SimplexListByDegree
-- return the index of the simplex in the SimplexListByDegree if it exists.
-- If it does not, return -1. 
indexOfSimplex :: (Ord a) => Simplex a -> SimplexListByDegree a -> Int 
indexOfSimplex _       (SimplexListByDegree _ []) = error "Improper SimplexListByDegree in the function indexOfSimplex."
indexOfSimplex simplex (SimplexListByDegree _ xs) = 
    let 
        index = elemIndex simplex xs 
    in 
        if index == Nothing then 
            -1 
        else 
            fromJust index

-- addSimplexToListOfSimplexListByDegree
-- Given a simplex and a list of SimplexListByDegree types,
-- return a list of SimplexListByDegree with the simplex added to the appropriate SimplexListByDegree.
addSimplexToListOfSimplexListByDegree :: Simplex a -> [SimplexListByDegree a] -> [SimplexListByDegree a]
addSimplexToListOfSimplexListByDegree (Simplex simplex) [] = [(SimplexListByDegree (length simplex) [(Simplex simplex)])]
addSimplexToListOfSimplexListByDegree (Simplex simplex) (x:xs)
    | (length simplex) /= (degreeOfSimplexListByDegree x) = x:(addSimplexToListOfSimplexListByDegree (Simplex simplex) xs)
    | otherwise = (addSimplexToSimplexListByDegree (Simplex simplex) x):xs

-- OrderedSimplexList
-- This data type is a list of SimplexListByDegree.
-- It is sorted by the degree of simplices and groups simplices of equal degree together.
-- note: the list should always be sorted by degree.
data OrderedSimplexList a = OrderedSimplexList [SimplexListByDegree a] deriving (Show, Eq)

-- Ordered instance for SimplexListByDegree type
-- Ordering given by degree of SimplexListByDegree.
instance (Ord a) => Ord (SimplexListByDegree a) where 
    compare (SimplexListByDegree m _) (SimplexListByDegree n _) = compare m n

-- streamToOrderedSimplex
-- Given a stream, (e.g. [1,2,3,(1,2),(1,3)]), transforms it
-- into a sorted OrderedSimplexList (e.g. [1: [1,2,3], 2: [(1,2), (1,3)]]).
streamToOrderedSimplexList :: (Ord a) => Stream a -> OrderedSimplexList a
streamToOrderedSimplexList (Simplices []) = error "Cannot convert non-initialized stream to ordered simplex list."
streamToOrderedSimplexList (Simplices xs) = OrderedSimplexList (sort (foldr (addSimplexToListOfSimplexListByDegree) [] xs))

