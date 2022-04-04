import Data.List (partition)
 

-- Source: http://www.rosettacode.org/wiki/Sorting_algorithms/Quicksort#Haskell
qsort :: Ord a => [a] -> [a] -- Ord: takes in datatypes that can be ordered, input and output are a list of Ord
qsort [] = [] -- return empty if empty input
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs where
    (ys, zs) = partition (< x) xs -- partition: picks values that fulfill condition (<x) --> head is selected as pivot