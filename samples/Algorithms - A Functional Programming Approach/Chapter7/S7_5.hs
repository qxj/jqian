module S7_5 where
-- to get access to the imported modules in Hugs do
-- :set -P../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts

import PQueue
import Graph
import Table
import List
import Ix

-- corrected as suggested by Herbert Kuchen
unionFind :: (Eq n, Ord n) => (n,n) -> Table n n -> (Bool,Table n n)
unionFind (x,y) t =
  --let xv = findTable t x
  --    yv = findTable t y
  let xv = findRoot t x 
      yv = findRoot t y
  in
    if (xv == yv)
    then (False,t)
    else (True,updTable (if yv<xv then (x,yv) else (y,xv)) t)

findRoot:: (Eq n) => Table n n -> n -> n
findRoot t x = let v = findTable t x
               in if v == x then v
               else findRoot t v

fillPQ :: (Ord n, Ord w, Ord c) => [(n,w,c)] -> PQueue (c,n,w) -> PQueue (c,n,w)
fillPQ [] pq           = pq
fillPQ ((x,y,w):es) pq = fillPQ es (enPQ (w,x,y) pq)

kruskal :: (Num w, Ix n, Ord w) => Graph n w -> [(w,n,n)]
kruskal g = kruskal' (fillPQ (edgesU g) emptyPQ)
                     (newTable [(x,x) | x<- nodes g])
                     [] 1
   where n             = length (nodes g)
         kruskal' pq t mst i 
             | i==n    = mst
             | otherwise = let e@(_,x,y)    = frontPQ pq
                               pq'          = dePQ pq
                               (updated,t') = unionFind (x,y) t
                           in if updated
                              then kruskal' pq' t'(e:mst) (i+1)
                              else kruskal' pq' t  mst     i

prim :: (Num w, Ix n, Ord w) => Graph n w -> [(w,n,n)]
prim g = prim' [n] ns []
         where (n:ns) = nodes g
               es = edgesU g
               prim' t [] mst = mst
               prim' t r mst 
                   = let e@(c,u',v') = minimum [(c,u,v)|(u,v,c)<-es,
                                                        elem u t, elem v r]
                     in prim' (v':t) (delete v' r) (e:mst)


g = mkGraph True (1,5) [(1,2,12),(1,3,34),(1,5,78),
                        (2,4,55),(2,5,32),
                        (3,4,61),(3,5,44),
                        (4,5,93)]
-- suggested modification by Herbert Kuchen to check that the original
-- version did not take transitive relation into account
g' = mkGraph True (1,5) [(1,2,13),(1,3,11),(1,5,78),
                        (2,4,12),(2,5,32),
                        (3,4,14),(3,5,44),
                        (4,5,93)]


{-   examples of evaluations and results 
? kruskal g
[(55, 2, 4), (34, 1, 3), (32, 2, 5), (12, 1, 2)]
? prim g
[(55, 2, 4), (34, 1, 3), (32, 2, 5), (12, 1, 2)]
-}
