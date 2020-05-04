-- | authors: Connor Ford, Jake Hauser

module DataCloud where

data Point a = Point a (Double, Double) deriving (Eq)

instance (Show a) => Show (Point a) where 
    show (Point name coords) = "{" ++ show name ++ "," ++ show coords ++ "}"

instance (Ord a) => Ord (Point a) where 
    compare (Point name1 _) (Point name2 _) = compare name1 name2