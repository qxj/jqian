module S6_3_1 where
-- Selection Sort
import List

-- naive implementation

ssort :: (Ord a) => [a] -> [a]
ssort [] = []
ssort xs = m : ssort (delete m xs)
           where m = minimum xs    

-- more efficient implementation

split :: (Ord a) => [a] -> a -> [a] -> [a]
split [] m r = m : (ssort r)
split (x:xs) m r = if x < m
                   then split xs x (m:r)
                   else split xs m (x:r)

ssort' [] = []
ssort' (x:xs) = split xs x []

ex = [3,1,4,1,5,9,2,8]

{-  Examples of evaluations and results 
    (with number of reductions given by hugs +s)

? ssort ex
[1, 1, 2, 3, 4, 5, 8, 9]
(358 reductions, 584 cells)
? ssort' ex
[1, 1, 2, 3, 4, 5, 8, 9]
(333 reductions, 572 cells)

-}
