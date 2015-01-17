--------------------------------------------------------------------------
-- Several algorithms
--------------------------------------------------------------------------
import Debug.Trace (trace)

--------------------------------------------------------------------------
-- Mergesort
--------------------------------------------------------------------------
mergesort :: (Ord a) => [a] -> [a]
mergesort l = ms $ runs l 
  where ms []       = []
        ms [xs]     = xs
        ms (x:y:zs) = merge (merge x y) (ms zs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

--------------------------------------------------------------------------
-- Bubblesort
--------------------------------------------------------------------------
bubblesort :: (Ord a) => [a] -> [a]
bubblesort l = case bubble False l of
                 (l',False) -> l'
                 (l',True ) -> bubblesort l'

bubble :: (Ord a) => Bool -> [a] -> ([a],Bool)
bubble t []  = ([],t)
bubble t [x] = ([x],t) 
bubble t (x:y:zs) | x > y     = let (l,t') = bubble True (x:zs) in (y:l,t')
                  | otherwise = let (l,t') = bubble t    (y:zs) in (x:l,t')

--------------------------------------------------------------------------
-- Quicksort
--------------------------------------------------------------------------
quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort lft ++ [x] ++ quicksort rgt
  where lft = [z | z <- xs, z <= x]
        rgt = [z | z <- xs, z >  x]

--------------------------------------------------------------------------
-- run + stepdown
--------------------------------------------------------------------------
runs :: (Ord a) => [a] -> [[a]]
runs [] = []
runs xs = let (r,zs) = run xs in r : runs zs
  where run []  = ([], [])
        run [x] = ([x],[])
        run (x:y:zs) | x > y     = ([x],y:zs)
                     | otherwise = let (r,ys) = run (y:zs) in (x:r,ys)

--------------------------------------------------------------------------
-- Measure of disorder
--------------------------------------------------------------------------
measure :: (Ord a) => [a] -> (Int,Int)
measure l = (10 * go l           `div` length l, 
             10 * go (reverse l) `div` length l)
  where go []  = 0
        go [x] = 0
        go (x:y:zs) | x > y     = 1 + go (y:zs)
                    | otherwise =     go (y:zs)


