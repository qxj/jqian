module S5_3 where

import Queue

test = foldr enqueue emptyQueue [1..10]

{-  examples of evaluations and results

? test
Q [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

-}
