module S7_3 where
-- to get access to the imported modules in Hugs do
-- :set -P../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts

import Graph
import Stack
import Queue

depthFirstSearch start g = dfs [start] []
  where
    dfs [] vis    = vis
    dfs (c:cs) vis 
      | elem c vis = dfs cs vis
      | otherwise  = dfs ((adjacent g c)++cs) (vis++[c])

depthFirstSearch' start g = reverse (dfs [start] [])
  where
   dfs [] vis     = vis
   dfs (c:cs) vis 
      | elem c vis = dfs cs vis
      | otherwise  = dfs ((adjacent g c)++cs) (c:vis)

depthFirstSearch'' start g = reverse (dfs (push start emptyStack) [])
 where
   dfs s vis 
    | (stackEmpty s)  = vis
    | elem (top s) vis = dfs (pop s) vis
    | otherwise       = let c = top s
                        in dfs (foldr push (pop s) (adjacent g c)) (c:vis)

breadthFirstSearch start g = reverse (bfs (enqueue start emptyQueue) [])
 where
  bfs q vis 
   | (queueEmpty q) = vis
   | elem (front q) vis = bfs (dequeue q) vis
   | otherwise  = let c = front q
                  in bfs (foldr enqueue (dequeue q) (adjacent g c))
                          (c:vis)

g = mkGraph True (1,6) [(1,2,0),(1,3,0),(1,4,0), 
                        (3,6,0),(5,4,0),(6,2,0),
                        (6,5,0) ]


{-   examples of evaluations and results 

? depthFirstSearch 1 g
[1, 2, 3, 6, 5, 4]
? depthFirstSearch' 1 g
[1, 2, 3, 6, 5, 4]
? depthFirstSearch'' 1 g
[1, 2, 3, 6, 5, 4]
? breadthFirstSearch 1 g
[1, 4, 3, 2, 6, 5]

-}
