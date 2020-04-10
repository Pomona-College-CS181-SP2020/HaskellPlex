-- | 
module SimplexStream where

-- | An example function.
test :: String
test = "hello world" 



data Simplex = Vertex Int | Edge Int Int

instance Show Simplex where 
    show (Vertex a) = show a
    show (Edge a b) = "(" ++ show a ++ "," ++ show b ++ ")"

data Stream = Simplicies [Simplex]

instance Show Stream where
    show (Simplicies []) = ""
    show (Simplicies (x:xs)) = show x ++ ", " ++ show (Simplicies xs)

initializeStream :: Stream 
initializeStream = Simplicies []

addVertex :: Stream -> Int -> Stream 
addVertex (Simplicies xs) x = Simplicies $ (Vertex x):xs

-- Returns True if the vertex is in the stream. Otherwise, False.
isVertexInStream :: Stream -> Int -> Bool
isVertexInStream (Simplicies [])   _ = False
isVertexInStream (Simplicies (x:xs)) v = 
    case x of
        Vertex z -> if z == v then True else isVertexInStream (Simplicies xs) v
        Edge a b -> isVertexInStream (Simplicies xs) v


-- if a vertex on the edge is not in the stream, you get the original stream returned.
addEdge :: Stream -> Int -> Int -> Stream 
addEdge stream@(Simplicies xs) a b = 
    let
        aInStream = isVertexInStream stream a
        bInStream = isVertexInStream stream b
    in
        if aInStream && bInStream then
            Simplicies $ (Edge a b):xs
        else 
            stream
