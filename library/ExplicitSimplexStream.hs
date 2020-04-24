-- | authors: Connor Ford, Jake Hauser

module ExplicitSimplexStream where

import Data.List (sort, subsequences)

import Data.Matrix

-- Alternative approach:
--      data Simplex = Simplex Int [Int]
-- where the first term is the length(order) of the simplex.
-- advantages: this allows for much faster lookup. i.e. when iterating over a list of simplex we would not have to compute lengths.
-- disadvantages: more complexity for storage.
--
-- Another note: We should be able to implement with for a generic type a, as long as we can define equality and ordering on a.
--
data Simplex a = Simplex [a]

instance (Show a) => Show (Simplex a) where 
    show (Simplex xs) = show xs

-- Sort simplex lists, then iterate over both simultaneously comparing elements.
-- note: requires that elements have an ordering.
-- O(n log(n) + m log (m) + n + m)
instance (Ord a) => Eq (Simplex a) where
    (Simplex []) == (Simplex []) = True
    (Simplex []) == (Simplex _ys) = False 
    (Simplex _xs) == (Simplex []) = False
    (Simplex xs) == (Simplex ys)
            | x /= y = False 
            | x == y = (Simplex rest_x) == (Simplex rest_y)
        where 
            (x:rest_x) = sort xs
            (y:rest_y) = sort ys


-- A stream is a list of simplices.
-- If the list is empty then the stream is non-initialized. If this is the case
-- then any operations on the stream will throw an error.
--
-- A simplicial complex is a non-empty set of finite sets closed under subsets.
data Stream a = Simplices [Simplex a]

instance (Show a) => Show (Stream a) where
    show (Simplices []) = ""
    show (Simplices (x:xs)) = show x ++ ", " ++ show (Simplices xs)


-- simplexInStream
-- Iterates over the stream and checks for equality with the given simplex.
-- efficiency: O(n)
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

-- True if first stream is a subcomplex of the second stream
isSubcomplex :: (Ord a) => Stream a -> Stream a -> Bool
isSubcomplex (Simplices []) _stream2 = error "Cannot find subcomplex of uninitialized stream."
isSubcomplex (Simplices [x]) stream2 = simplexInStream x stream2 -- base case
isSubcomplex (Simplices (x:xs)) stream2 = 
    if not (simplexInStream x stream2) then 
        False 
    else 
        isSubcomplex (Simplices xs) stream2

-- TODO: implement more efficiently by looking at higher-order simplices
-- Two streams are equivalent if they contain identical simplices.
instance (Ord a) => Eq (Stream a) where 
    stream1 == stream2 = 
        isSubcomplex stream1 stream2 && isSubcomplex stream2 stream1

initializeStream :: Stream a 
initializeStream = Simplices [(Simplex [])] -- initialize with null cell

-- Default filtration value 0.
addVertex :: Stream a -> a -> Stream a
addVertex (Simplices xs) x = Simplices $ (Simplex [x]):xs

-- Returns True if the vertex is in the stream. Otherwise, False.
isVertexInStream :: (Ord a) => Stream a -> a -> Bool
isVertexInStream (Simplices [])     _ = error "Cannot find vertex in a non-initialized stream."
isVertexInStream (Simplices [x])   v = (vertexLift x) == v
isVertexInStream (Simplices (x:xs)) v = 
    case x of
        Simplex [z] -> if z == v then True else isVertexInStream (Simplices xs) v
        Simplex _   -> isVertexInStream (Simplices xs) v


-- Given a simplex return a list of all subcomplexes.
-- note: this includes the trivial subcomplexes [] and the inputted simplex
getSubcomplexes :: Simplex a -> [Simplex a]
getSubcomplexes (Simplex s) = map (\x -> (Simplex x)) (subsequences s)



-- addSimplex adds a simplex and all sub-complexes to the stream if not already present.
-- requirement: all names in simplex must be unique
-- default filtration value 0.
addSimplex :: (Ord a) => Stream a -> Simplex a -> Stream a
addSimplex (Simplices []) _ = error "Cannot add a simplex to a non-initialized stream."
addSimplex (Simplices simps) simplex = 
    Simplices (foldl (\simplicesAccumulator spx -> if simplexInStream spx (Simplices simplicesAccumulator) then simplicesAccumulator else spx:simplicesAccumulator) simps (getSubcomplexes simplex))


-- get number of simplices in stream
getSize :: Stream a -> Int
getSize (Simplices []) = error "Cannot get size of a non-initialized stream."
getSize (Simplices l) = length l

-- given a simplex, determine if it is a vertex
isVertex :: Simplex a  -> Bool
isVertex (Simplex [_x]) = True 
isVertex _          = False

-- get the number of vertices
numVertices :: Stream a -> Int 
numVertices (Simplices [])    = error "Cannot get the number of vertices for a non-initialized stream."
numVertices (Simplices simps) = foldl (\acc x -> if (isVertex x) then acc + 1 else acc) 0 simps

-- get value from vertex
vertexLift :: Simplex a -> a
vertexLift (Simplex [x]) = x 
vertexLift _             = error "the vertexLift method only takes vertices as input."

-- get verticies 
getVertices :: Stream a -> [a]
getVertices (Simplices l) = 
    foldl(\acc simplex -> 
        if isVertex simplex then 
            (vertexLift simplex):acc 
        else 
            acc
    ) [] l


data BettiVector = BettiVector [Int]

instance Show BettiVector where
    show (BettiVector []) = "()"
    show (BettiVector vs) = "(" ++ (foldl (\acc x -> acc ++ (show x)) "" vs) ++ ")"

-- persistence
-- Algorithm: 
-- Given a simplex stream, and a field coefficient p corresponding to Z/pZ we will return betti numbers (b_0, b_1, ..., b_n) where n+1 equals the degree of the highest order simplex in the stream. 
-- (1) Get simplex lists ordered by the degree of simplices
-- (2) Get boundary maps D_0, ..., D_{n-1} such that:
-- 0 <-- C_0 <-- C_1 <-- ... <-- C_{n-1} <-- C_n <-- 0
--  D_{-1}   D_0     D_1                D_{n-1}
-- (3) Compute dimensions of the Homologies:
-- dim H_0 = dim Ker 0 - dim Im D_0
-- dim H_1 = dim Ker D_0 - dim Im D_1 
-- ...
-- dim H_n = dim Ker D_{n-1} - dim Im 0
-- (4) return betti profile (dim H_0, dim H_1, ..., dim H_n)

-- TODO: define show, equality, and ordering.
data SimplexListByDegree a = SimplexListByDegree Int [Simplex a] deriving (Show)

instance (Ord a) => Eq (SimplexListByDegree a) where 
    (SimplexListByDegree a xs) == (SimplexListByDegree b ys) = (a == b) && (Simplices xs == Simplices ys)

degreeOfSimplexListByDegree :: SimplexListByDegree a -> Int 
degreeOfSimplexListByDegree (SimplexListByDegree x _) = x

-- does not check length
addSimplexToSimplexListByDegree :: Simplex a -> SimplexListByDegree a -> SimplexListByDegree a
addSimplexToSimplexListByDegree simp (SimplexListByDegree len xs) = SimplexListByDegree len (simp:xs)

data OrderedSimplexList a = OrderedSimplexList [SimplexListByDegree a] deriving (Show, Eq)

instance (Ord a) => Ord (SimplexListByDegree a) where 
    compare (SimplexListByDegree m _) (SimplexListByDegree n _) = compare m n

addSimplexToOrderedSimplexList :: Simplex a -> [SimplexListByDegree a] -> [SimplexListByDegree a]
addSimplexToOrderedSimplexList (Simplex simp) [] = [(SimplexListByDegree (length simp) [(Simplex simp)])]
addSimplexToOrderedSimplexList (Simplex simp) (x:xs)
    | (length simp) /= (degreeOfSimplexListByDegree x) = x:(addSimplexToOrderedSimplexList (Simplex simp) xs)
    | (length simp) == (degreeOfSimplexListByDegree x) = (addSimplexToSimplexListByDegree (Simplex simp) x):xs
    
-- streamToOrderedSimplex
-- transforms a stream (e.g. [1,2,3,(1,2),(1,3)]) 
-- into an ordered simplex list (e.g. [1: [1,2,3], 2: [(1,2), (1,3)]]).
streamToOrderedSimplexList :: (Ord a) => Stream a -> OrderedSimplexList a
streamToOrderedSimplexList (Simplices []) = error "Cannot convert non-initialized stream to ordered simplex list."
streamToOrderedSimplexList (Simplices xs) = OrderedSimplexList (sort (foldr (addSimplexToOrderedSimplexList) [] xs))



removeListElement :: [a] -> Int -> [a]
removeListElement [] _ = []
removeListElement (x:xs) n = if n == 0 then xs else x:(removeListElement xs (n-1))


-- First argument C_k+1
-- Second argument C_k
-- matrix dimensions: len(C_k) = num rows, len(C_k+1) = num cols
-- algorithm: 
-- Given element of C_k+1, y, remove i'th element from y (starting with index zero) to get element in C_k called x. 
-- Then the value in the matrix at row idxOf(x) and column idxOf(y) is (-1)^i.
getBoundaryMap :: SimplexListByDegree a -> SimplexListByDegree a -> Matrix Int
getBoundaryMap list1 (SimplexListByDegree n (x:xs)) = undefined

persistence :: Stream a -> Int -> BettiVector
persistence stream field = undefined