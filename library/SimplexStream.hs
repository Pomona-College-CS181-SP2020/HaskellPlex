-- | authors: Connor Ford, Jake Hauser

module SimplexStream where

import Data.List (sort)


data Simplex = Simplex [Int]

instance Show Simplex where 
    show (Simplex xs) = show xs

instance Eq Simplex where
    (Simplex []) == (Simplex []) = True
    (Simplex xs) == (Simplex ys)
            | xs_sorted == [] && ys_sorted /= []   = False
            | xs_sorted /= []  && ys_sorted == []  = False
            | x /= y = False 
            | x == y = (Simplex rest_x) == (Simplex rest_y)
        where 
            xs_sorted@(x:rest_x) = sort xs
            ys_sorted@(y:rest_y) = sort ys



data Stream = Simplicies [Simplex]

instance Show Stream where
    show (Simplicies []) = ""
    show (Simplicies (x:xs)) = show x ++ ", " ++ show (Simplicies xs)

-- TODO: implement more efficiently by looking at higher-order simplices
instance Eq Stream where 
    (Simplicies ls) == (Simplicies ts) = ls == ts -- WRONG!!

initializeStream :: Stream 
initializeStream = Simplicies [(Simplex [])] -- initialize with null cell

addVertex :: Stream -> Int -> Stream 
addVertex (Simplicies xs) x = Simplicies $ (Simplex [x]):xs

-- Returns True if the vertex is in the stream. Otherwise, False.
isVertexInStream :: Stream -> Int -> Bool
isVertexInStream (Simplicies [])     _ = False
isVertexInStream (Simplicies (x:xs)) v = 
    case x of
        Simplex [z] -> if z == v then True else isVertexInStream (Simplicies xs) v
        Simplex _   -> isVertexInStream (Simplicies xs) v


-- if a vertex on the edge is not in the stream, you get the original stream returned.
addEdge :: Stream -> Int -> Int -> Stream 
addEdge stream@(Simplicies xs) a b = 
    let
        aInStream = isVertexInStream stream a
        bInStream = isVertexInStream stream b
    in
        if aInStream && bInStream then
            Simplicies $ (Simplex [a,b]):xs
        else 
            stream


-- get number of simplicies in stream
getSize :: Stream -> Int 
getSize (Simplicies l) = length l

-- given a simplex, determine if it is a vertex
isVertex :: Simplex -> Bool
isVertex (Simplex [x]) = True 
isVertex _          = False

-- get value from vertex
vertexLift :: Simplex -> Int
vertexLift (Simplex [x]) = x 
vertexLift _             = error "the vertexLift method only takes vertices as input."

-- get verticies 
getVertices :: Stream -> [Int]
getVertices (Simplicies l) = foldl (\acc simplex -> if isVertex simplex then (vertexLift simplex):acc else acc) [] l