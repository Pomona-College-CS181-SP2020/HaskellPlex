-- | authors: Connor Ford, Jake Hauser

module ExplicitSimplexStream where

import Data.List (sort, subsequences, delete, intercalate)

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
    (Simplex []) == (Simplex _ys) = False 
    (Simplex _xs) == (Simplex []) = False
    (Simplex xs) == (Simplex ys)
            | x /= y = False 
            | x == y = (Simplex rest_x) == (Simplex rest_y)
        where 
            (x:rest_x) = sort xs
            (y:rest_y) = sort ys

-- data type: Stream a
-- A Stream is a non-trivial list of simplices.
-- note: If the list is empty then the stream is non-initialized. If this is the case
-- then any operations on the stream throw an error.
-- note: An equivalent concept is the simplicial complex. A simplicial complex is a non-empty set of finite sets closed under subsets. We require that these simplex streams satisfy this same property.
data Stream a = Simplices [Simplex a]

instance (Show a) => Show (Stream a) where
    show (Simplices []) = error "Cannot display a non-initialized Stream."
    show (Simplices xs) = intercalate ", " (map show xs)

-- TODO: implement more efficiently by looking at higher-order simplices
-- Two streams are equivalent if they contain identical simplices.
instance (Ord a) => Eq (Stream a) where 
    stream1 == stream2 = 
        isSubcomplex stream1 stream2 && isSubcomplex stream2 stream1

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


-- get value from simplex
simplexLift :: Simplex a -> [a]
simplexLift (Simplex x) = x 

getSimplicesSizeN :: (Ord a) => Stream a -> Int -> [Simplex a]
getSimplicesSizeN (Simplices []) _ = error "Cannot get simplices for uninitialized stream."
getSimplicesSizeN (Simplices (x:xs)) n = 
    if length (simplexLift x) == n then 
        x:(getSimplicesSizeN (Simplices (xs)) n) 
    else 
        getSimplicesSizeN (Simplices (xs)) n

isSimplexInSimplexList :: (Ord a) => Simplex a -> [Simplex a] -> Bool 
isSimplexInSimplexList s [] = False 
isSimplexInSimplexList s (x:xs) = if x == s then True else (isSimplexInSimplexList s xs) || False

-- determines if a simplex is not a sub-simplex of any other simplices in the stream.
-- algorithm: get vertices.
-- iterate over vertices, adding them to simplex.
-- compare with simplices of length simplex + 1.
isMaxSimplex :: (Ord a) => Stream a -> Simplex a -> Bool
isMaxSimplex stream (Simplex []) = False -- base case / trivial case. 
isMaxSimplex stream (Simplex xs) = 
    let 
        len = length xs 
        vertices = getSimplicesSizeN stream 1
        nPlusOneSimplices = getSimplicesSizeN stream (len + 1)
    in 
        foldl (\acc x -> if isSimplexInSimplexList (Simplex (x:xs)) nPlusOneSimplices then False else acc) True (map vertexLift vertices)

-- when subtracting, need to make sure that we maintain a valid simplicial complex. So in the inclusion maps, we can only remove from the top.
subtractSimplex :: (Ord a) => Stream a -> Simplex a -> Stream a 
subtractSimplex (Simplices []) _ = error "Cannot subtract from an uninitialized stream."
subtractSimplex (Simplices (x:xs)) s = 
    if isMaxSimplex (Simplices (x:xs)) s then
        Simplices (delete s (x:xs))
    else 
        (Simplices (x:xs))

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

removeSimplexElement :: Simplex a -> Int -> [a]
removeSimplexElement (Simplex []) _ = []
removeSimplexElement (Simplex (x:xs)) n = if n == 0 then xs else x:(removeSimplexElement (Simplex xs) (n-1))

-- second argument should be zero.
-- necessitates that simplex is in simplexlist
indexOfSimplex :: (Ord a) => Simplex a -> Int -> SimplexListByDegree a -> Int 
indexOfSimplex simp k (SimplexListByDegree n []) = error "Improper SimplexListByDegree in indexOfSimplex"
indexOfSimplex simp k (SimplexListByDegree n (x:xs)) = if x == simp then k else indexOfSimplex simp (k+1) (SimplexListByDegree n xs)

