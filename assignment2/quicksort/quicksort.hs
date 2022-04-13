import Data.List (partition)
 

-- Source: http://www.rosettacode.org/wiki/Sorting_algorithms/Quicksort#Haskell
-- This implementation is efficient as it does not have excessive method calls. 
-- This implementation is least efficient, if the list is already sorted as each partition will split only one element.
-- Picking a different pivot element could help with that. However since lists are implemented as linked lists, it saves computation effort to choose the first element as pivot.
qsort :: Ord a => [a] -> [a] -- Ord: takes in datatypes that can be ordered, input and output are a list of Ord
qsort [] = [] -- return empty if empty input
-- separates list in head and tail --> head is selected as pivot
qsort (x:xs) = qsort smaller ++ [x] ++ qsort greater -- recursive call on the two partitions, adds together the sorted parts with pivot in middle
    where
        (smaller, greater) = partition (< x) xs -- partition: separates values that fulfill condition (<x) from others

main = do
    let toSort = [3,4,2,6,8,5,23,4,8,3,2,6,7,7]
    print $ qsort toSort
    let toSortFloat = [3.4, 4.1, 4.01, 6.0, 7.1, 10.0, 9.9999, 9.99]
    print $ qsort toSortFloat
    let toSortStr = ["a", "e", "f", "c", "a", "d", "g", "b", "abcd"]
    print $ qsort toSortStr
