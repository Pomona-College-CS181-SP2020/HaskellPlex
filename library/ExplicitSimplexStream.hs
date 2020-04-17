-- | authors: Connor Ford, Jake Hauser

module ExplicitSimplexStream where

import Data.List (sort, subsequences)

-- Alternative approach:
--      data Simplex = Simplex Int [Int]
-- where the first term is the length(order) of the simplex.
-- advantages: this allows for much faster lookup. i.e. when iterating over a list of simplex we would not have to compute lengths.
-- disadvantages: more complexity for storage.
--
-- Another note: We should be able to implement with for a generic type a, as long as we can define equality on a. We may not require ordering (Ord) although Ord could make implementations quicker.
--
data Simplex = Simplex [Int]

instance Show Simplex where 
    show (Simplex xs) = show xs

-- Sort simplex lists, then iterate over both simultaneously comparing elements.
-- note: requires that elements have an ordering.
-- O(n log(n) + m log (m) + n + m)
instance Eq Simplex where
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
data Stream = Simplices [Simplex]

instance Show Stream where
    show (Simplices []) = ""
    show (Simplices (x:xs)) = show x ++ ", " ++ show (Simplices xs)


-- simplexInStream
-- Iterates over the stream and checks for equality with the given simplex.
-- efficiency: O(n)
simplexInStream :: Simplex -> Stream -> Bool 
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
isSubcomplex :: Stream -> Stream -> Bool
isSubcomplex (Simplices []) _stream2 = error "Cannot find subcomplex of uninitialized stream."
isSubcomplex (Simplices [x]) stream2 = simplexInStream x stream2 -- base case
isSubcomplex (Simplices (x:xs)) stream2 = 
    if not (simplexInStream x stream2) then 
        False 
    else 
        isSubcomplex (Simplices xs) stream2

-- TODO: implement more efficiently by looking at higher-order simplices
-- Two streams are equivalent if they contain identical simplices.
instance Eq Stream where 
    stream1 == stream2 = 
        isSubcomplex stream1 stream2 && isSubcomplex stream2 stream1

initializeStream :: Stream 
initializeStream = Simplices [(Simplex [])] -- initialize with null cell

addVertex :: Stream -> Int -> Stream 
addVertex (Simplices xs) x = Simplices $ (Simplex [x]):xs

-- Returns True if the vertex is in the stream. Otherwise, False.
isVertexInStream :: Stream -> Int -> Bool
isVertexInStream (Simplices [])     _ = error "Cannot find vertex in a non-initialized stream."
isVertexInStream (Simplices [x])   v = (vertexLift x) == v
isVertexInStream (Simplices (x:xs)) v = 
    case x of
        Simplex [z] -> if z == v then True else isVertexInStream (Simplices xs) v
        Simplex _   -> isVertexInStream (Simplices xs) v


-- Given a simplex return a list of all subcomplexes.
-- note: this includes the trivial subcomplexes [] and the inputted simplex
getSubcomplexes :: Simplex -> [Simplex]
getSubcomplexes (Simplex s) = map (\x -> (Simplex x)) (subsequences s)



-- addSimplex adds a simplex and all sub-complexes to the stream if not already present.
-- requirement: all names in simplex must be unique
addSimplex :: Stream -> Simplex -> Stream
addSimplex (Simplices []) _ = error "Cannot add a simplex to a non-initialized stream."
addSimplex (Simplices simps) simplex = 
    Simplices (foldl (\simplicesAccumulator spx -> if simplexInStream spx (Simplices simplicesAccumulator) then simplicesAccumulator else spx:simplicesAccumulator) simps (getSubcomplexes simplex))


-- get number of simplices in stream
getSize :: Stream -> Int 
getSize (Simplices []) = error "Cannot get size of a non-initialized stream."
getSize (Simplices l) = length l

-- given a simplex, determine if it is a vertex
isVertex :: Simplex -> Bool
isVertex (Simplex [_x]) = True 
isVertex _          = False

-- get the number of vertices
numVertices :: Stream -> Int 
numVertices (Simplices [])    = error "Cannot get the number of vertices for a non-initialized stream."
numVertices (Simplices simps) = foldl (\acc x -> if (isVertex x) then acc + 1 else acc) 0 simps

-- get value from vertex
vertexLift :: Simplex -> Int
vertexLift (Simplex [x]) = x 
vertexLift _             = error "the vertexLift method only takes vertices as input."

-- get verticies 
getVertices :: Stream -> [Int]
getVertices (Simplices l) = 
    foldl(\acc simplex -> 
        if isVertex simplex then 
            (vertexLift simplex):acc 
        else 
            acc
    ) [] l