module HeapSort where

import PQueue

mkPQ  :: (Ord a) => [a] -> PQueue a
mkPQ xs  = foldr enPQ emptyPQ xs
 
hsort :: (Ord a) => [a] -> [a]
hsort xs = hsort' (mkPQ xs)
           where hsort' pq 
                   | (pqEmpty pq) = []
                   | otherwise    = (frontPQ pq):(hsort' (dePQ pq))

ex = [3,1,4,1,5,9,2,8]

{-  Examples of evaluations and results 

? hsort ex
[1, 1, 2, 3, 4, 5, 8, 9]
-}
