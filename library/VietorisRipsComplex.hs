-- | authors: Connor Ford, Jake Hauser

module VietorisRipsComplex where

import ExplicitSimplexStream
import DataCloud


vietorisRips :: [Point a] -> Double -> Stream a -> Stream a
vietorisRips (x:xs) r stream = 
    let
        points_in_neighborhood = filter (inNeighborhood r x) xs
    in 
        undefined