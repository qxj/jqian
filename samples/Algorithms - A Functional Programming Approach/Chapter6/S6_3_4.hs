module S6_3_4 where

import List

-- Merge Sort

-- naive implementation
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort l = merge (msort l1)(msort l2)
           where l1 = (take k l )
                 l2 = (drop k l)
                 k  = (length l) `div` 2 

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys) | (x<=y)    = x : (merge xs b)
                        | otherwise = y : (merge a ys)

-- improved implementation
msort' :: (Ord a) => [a] -> [a]
msort' [] = []
msort' [x] = [x]
msort' l = merge (msort' l1) (msort' l2)
           where l1 = (take k l )
                 l2 = (drop k l)
                 k  = (length l) `div` 2 

-- more efficient implementation

split :: (Ord a) => [a] -> [[a]]
split []     = []
split (x:xs) = [x] : split xs

mergepairs :: (Ord a) => [[a]] -> [[a]]
mergepairs []           = []
mergepairs x@[l]        = x
mergepairs (l1:l2:rest) = (merge l1 l2) : (mergepairs rest)

msort'' :: (Ord a) => [a] -> [a]
msort'' l = ms (split l)
         where ms [r] = r
               ms l   = ms (mergepairs l)

ex = [3,1,4,1,5,9,2,8]

{-  Examples of evaluations and results 
    (with number of reductions given by hugs +s)

? msort ex
[1, 1, 2, 3, 4, 5, 8, 9]
(638 reductions, 1022 cells)
? msort' ex
[1, 1, 2, 3, 4, 5, 8, 9]
(627 reductions, 994 cells)
? msort'' ex
[1, 1, 2, 3, 4, 5, 8, 9]
(199 reductions, 380 cells)

-}
