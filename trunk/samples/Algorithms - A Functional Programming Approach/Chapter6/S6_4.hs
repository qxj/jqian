module S6_4 where
-- to get access to the imported modules in Hugs do
-- :set -P../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts
import PQueue  
import BinTree

-- Heap Sort

mkPQ  :: (Ord a) => [a] -> PQueue a
mkPQ xs  = foldr enPQ emptyPQ xs
 
hsort :: (Ord a) => [a] -> [a]
hsort xs = hsort' (mkPQ xs)
           where hsort' pq 
                   | (pqEmpty pq) = []
                   | otherwise    = (frontPQ pq):(hsort' (dePQ pq))


-- Tree sort

tsort xs = (inorder . buildTree) xs


ex = [3,1,4,1,5,9,2,8]

{-  Examples of evaluations and results 
    (with number of reductions given by hugs +s)

? hsort ex
[1, 1, 2, 3, 4, 5, 8, 9]
(437 reductions, 850 cells)
? tsort ex
[1, 2, 3, 4, 5, 8, 9]
(210 reductions, 432 cells)

-}
