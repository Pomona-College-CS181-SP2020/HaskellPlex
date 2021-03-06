-- | authors: Connor Ford, Jake Hauser

module Persistence where 

import Data.List (intercalate)
import Numeric.LinearAlgebra
import Data.List.HT (mapAdjacent)
import ExplicitSimplexStream

-- Betti Vector
-- This data type is equivalent to a list of Ints. It represents the betti vector.
-- For a betti vector of length n, all betti numbers b_i such that i >= n are assumed to be 0.
data BettiVector = BettiVector [Int] deriving (Eq)

instance Show BettiVector where
    show (BettiVector []) = "()"
    show (BettiVector vs) = "(" ++ intercalate ", " (map show vs) ++ ")"

-- getBoundaryMapHelper (see: getBoundaryMap)
-- First argument is SimplexListByDegree representing C_k
-- Second argument is SimplexListByDegree representing C_k+1
-- Third argument should initially be the length of the x in (x:xs) for C_k+1. It it a counter for removing elements from x.
-- Fourth argument should initially be zero. It is a counter for the index of x in (x:xs).
-- Fifth argument should be the zero matrix for _correct_ boundary map calculations.
getBoundaryMapHelper :: (Ord a) => SimplexListByDegree a -> SimplexListByDegree a -> Int -> Int -> Matrix Double -> Matrix Double
getBoundaryMapHelper _     (SimplexListByDegree _ [])     _ _ mat = mat
getBoundaryMapHelper list1 (SimplexListByDegree n (_:xs)) 0 k mat = getBoundaryMapHelper list1 (SimplexListByDegree n (xs)) n (k+1) mat
getBoundaryMapHelper list1 (SimplexListByDegree n (x:xs)) m k mat = 
    let 
        x_dim = indexOfSimplex (Simplex (removeSimplexElement x (m-1))) list1
        y_dim = k
        val = (-1)^(m-1)
        updatedMatrix = accum mat (\a _ -> a) [((x_dim,y_dim), val)] -- use accumulator to change values of matrix
    in 
        getBoundaryMapHelper list1 (SimplexListByDegree n (x:xs)) (m-1) k updatedMatrix

-- getBoundaryMap
-- First argument C_k
-- Second argument C_k+1
-- return matrix dimensions: len(C_k) = num rows, len(C_k+1) = num cols
-- algorithm: 
-- Given element of C_k+1, y, remove i'th element from y (starting with index zero) to get element in C_k called x. 
-- Then the value in the matrix at row idxOf(x) and column idxOf(y) is (-1)^i.
getBoundaryMap :: (Ord a) => SimplexListByDegree a -> SimplexListByDegree a -> Matrix Double
getBoundaryMap list1@(SimplexListByDegree _ simps1) list2@(SimplexListByDegree n simps2) = 
    let 
        zero_matrix = matrix (length simps2) (replicate ((length simps1)*(length simps2)) 0)
    in
        getBoundaryMapHelper list1 list2 n 0 zero_matrix

-- getHomologyDimension
-- first arg is D_i+1
-- second arg is D_i 
-- third argument k (or i in the below illustration)
-- want to find: dim ker D_i - dim im D_i+1
-- algorithm to find ker and im:
-- (i) rref matrix 
-- (ii) # of non-zero rows correspond to rnk of original matrix.
-- (iii) # of columns with leading 1’s correspond to the columns of original matrix that span image.
-- (iv) dim ker = (ii) - (iii)
getHomologyDimension :: Matrix Double -> Matrix Double -> Int -> Int 
getHomologyDimension m1 m2 k = 
    let 
        m1_rank = rank m1 
        m2_columns = cols m2 
        m2_rank = rank m2 
        m2_nullity = m2_columns - m2_rank
    in 
        if k > 0 then 
            m2_nullity - m1_rank 
        else 
            m2_columns - m1_rank

-- persistenceHelper (see: persistence)
-- First argument is a list of matrices representing a sequence of boundary maps.
-- Second argument is k representing the k'th homology.
persistenceHelper :: [Matrix Double] -> Int -> [Int]
persistenceHelper []       _ = []
persistenceHelper [x]      k = [getHomologyDimension ((ident 1) - (ident 1)) x k] -- base case for last boundary map
persistenceHelper (x:y:xs) k = (getHomologyDimension y x k):(persistenceHelper (y:xs) (k+1))

-- persistence
-- The persistence algorithm computes the BettiVector for s simplex stream. The algorithm is as follows: 
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
-- TODO: implement with rref mod p where Field = Z/pZ.
persistence :: (Ord a) => Stream a -> Int -> BettiVector
persistence stream field = 
    let 
        (OrderedSimplexList l) = streamToOrderedSimplexList stream 
    in 
        BettiVector $ persistenceHelper (mapAdjacent getBoundaryMap l) 0