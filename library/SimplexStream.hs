-- | 
module SimplexStream where

-- | An example function.
test :: String
test = "hello world" 



data Simplex = Vertex Int | Edge Int Int

instance Show Simplex where 
    show (Vertex a) = show a
    show (Edge a b) = "(" ++ show a ++ "," ++ show b ++ ")"

data Stream = EmptyStream | Simplicies [Simplex]

instance Show Stream where
    show EmptyStream = ""
    show (Simplicies []) = ""
    show (Simplicies (x:xs)) = show x ++ ", " ++ show (Simplicies xs)

initializeStream :: Stream 
initializeStream = EmptyStream

addVertex :: Stream -> Int -> Stream 
addVertex EmptyStream x = Simplicies [Vertex x]
addVertex (Simplicies xs) x = Simplicies $ (Vertex x):xs

addEdge :: Stream -> Int -> Int -> Stream 
addEdge EmptyStream a b = Simplicies [Edge a b]
addEdge (Simplicies xs) a b = Simplicies $ (Edge a b):xs
