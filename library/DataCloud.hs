-- | authors: Connor Ford, Jake Hauser

module DataCloud where

data Point a = Point a (Double, Double) deriving (Eq)

instance (Show a) => Show (Point a) where 
    show (Point name coords) = "{" ++ show name ++ "," ++ show coords ++ "}"

instance (Ord a) => Ord (Point a) where 
    compare (Point name1 _) (Point name2 _) = compare name1 name2

euclideanDistance :: (Double, Double) -> (Double, Double) -> Double 
euclideanDistance (a,b) (c,d) = sqrt $ (a-c)^2 + (b-d)^2

inNeighborhood :: Double -> Point a -> Point a -> Bool
inNeighborhood r (Point name1 p1) (Point name2 p2) = (euclideanDistance p1 p2) <= r