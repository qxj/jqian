module S7_4 where

import Graph

inDegree g n  = length [t | v<-nodes g, t<-adjacent g v, (n==t)]

topologicalSort g = tsort [n | n<-nodes g , (inDegree g n == 0)] [] 
  where
    tsort [] r      = r
    tsort (c:cs) vis  
     | elem c vis = tsort cs vis
     | otherwise  = tsort cs (c:(tsort (adjacent g c) vis))

g = mkGraph True (1,6) [(1,2,0),(1,3,0),(1,4,0), 
                        (3,6,0),(5,4,0),(6,2,0),
                        (6,5,0)]

data Courses = Maths | Theory | Languages | Programming | Concurrency
             | Architecture | Parallelism
    deriving (Eq,Ord,Enum,Ix,Show)

cg = mkGraph True (Maths,Parallelism) [(Maths,Theory,1),
                                       (Languages,Theory,1),
                                       (Programming,Languages,1),
                                       (Programming,Concurrency,1),
                                       (Concurrency,Parallelism,1),
                                       (Architecture,Parallelism,1)]

{-   examples of evaluations and results 
? topologicalSort g
[1, 3, 6, 5, 4, 2]
? topologicalSort cg
[Architecture, Programming, Concurrency, Parallelism, Languages, Maths, Theory]
-}
