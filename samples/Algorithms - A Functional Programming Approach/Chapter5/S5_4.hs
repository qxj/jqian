module S5_4 where
import PQueue

pq = foldr enPQ emptyPQ [3,1,7,2,9]

v = (frontPQ pq, dePQ pq)

{-  examples of evaluations and results

? pq
PQ (HP 1 2 (HP 2 2 (HP 9 1 EmptyHP EmptyHP) (HP 7 1 EmptyHP EmptyHP)) (HP 3 1 EmptyHP EmptyHP))
? v
(1, PQ (HP 2 2 (HP 9 1 EmptyHP EmptyHP) (HP 3 1 (HP 7 1 EmptyHP EmptyHP) EmptyHP)))

-}
