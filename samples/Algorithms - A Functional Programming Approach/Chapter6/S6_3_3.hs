module S6_3_3 where

-- QuickSort

-- naive implementation
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (pivot:rest) = qsort lower ++ [pivot] ++ qsort upper
    where lower = [ x | x <- rest, x<= pivot]
          upper = [ x | x <- rest, x > pivot] 

-- improved implementation
qsort' :: (Ord a) => [a] -> [a] -> [a]
qsort' [] s = s
qsort' (pivot:rest) s = qsort' lower (pivot : (qsort' upper s))
    where lower = [ x | x <- rest, x<= pivot]
          upper = [ x | x <- rest, x > pivot] 

-- more efficient implementation
--   note this is different from the book version
--   it is the correct and more efficient version 
qsort'' l = qs l []
    where qs []  s          = s
          qs [x] s          = (x:s)
          qs (pivot:rest) s = split pivot rest [] [] s
          split pivot [] lower upper s     
            = qs lower (pivot : (qs upper s))
          split pivot (x:xs) lower upper s 
            = if x < pivot
              then split pivot xs (x:lower) upper s
              else split pivot xs lower (x:upper) s

ex = [3,1,4,1,5,9,2,8]

{-  Examples of evaluations and results 
    (with number of reductions given by hugs +s)

? qsort ex
[1, 1, 2, 3, 4, 5, 8, 9]
(380 reductions, 679 cells)
? qsort' ex []
[1, 1, 2, 3, 4, 5, 8, 9]
(351 reductions, 624 cells)
? qsort'' ex
[1, 1, 2, 3, 4, 5, 8, 9]
(160 reductions, 389 cells)

-}
