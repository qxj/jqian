module S7_6 where
-- to get access to the imported modules in Hugs do
-- :set -P../Chapter5:{Hugs}/lib:{Hugs}/lib/hugs:{Hugs}/lib/exts

import Graph
import Set

data Tree n = NodeGT n [Tree n]
    deriving Show



depthFirstTree :: (Num w,Ix n) => n -> Graph n w -> Tree n
depthFirstTree s g = head (snd (dfst g (emptySet,[]) s))

dfst::(Ix n,Num w) => Graph n w -> (Set n , [Tree n]) -> n
                      -> (Set n , [Tree n])
dfst g (vs,ts) n
    | inSet n vs  = (vs,ts)
    | otherwise   = (vs',(NodeGT n ts'):ts)
    where (vs',ts') = foldl (dfst g)
                            (addSet n vs, [])
                            (adjacent g n)

dfsforest :: (Num w,Ix n) => Graph n w -> [Tree n]
dfsforest g = snd (foldl (dfst g) (emptySet,[]) (nodes g))

generate g v = NodeGT v (map (generate g) (adjacent g v))

prune :: Ix n => [Tree n] -> [Tree n]
prune ts   = snd (prune' emptySet ts)
  where
    prune' m [] = (m,[])
    prune' m ((NodeGT v ts):us)
       | inSet v m  = prune' m us
       | otherwise  = let (m',as) = prune' (addSet v m) ts
                          (m'',bs)= prune' m' us
                      in (m'',(NodeGT v as):bs)



kldfsforest g = prune (map (generate g) (nodes g))

preorderT (NodeGT v l)= v:(concat (map preorderT l))

kldfs v g = preorderT (head (prune (map (generate g) [v])))
               

g = mkGraph True (1,8) [(1,2,0),(1,3,0),(1,4,0), 
                        (3,6,0),(5,4,0),(6,2,0),
                        (6,5,0) ]

{-   examples of evaluations and results 

? depthFirstTree 1 g
NodeGT 1 [NodeGT 3 [NodeGT 6 [NodeGT 5 [NodeGT 4 []]]], NodeGT 2 []]
? dfsforest g
[NodeGT 8 [], NodeGT 7 [], NodeGT 1 [NodeGT 3 [NodeGT 6 [NodeGT 5 [NodeGT 4 []]]], NodeGT 2 []]]
? kldfs 1 g
[1, 2, 3, 6, 5, 4]
? kldfsforest g
[NodeGT 1 [NodeGT 2 [], NodeGT 3 [NodeGT 6 [NodeGT 5 [NodeGT 4 []]]]], NodeGT 7 [], NodeGT 8 []]

-}
