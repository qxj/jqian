module S9_4 where

import Dynamic
import Array

type ObstCoord  = (Int,Int)
type ObstEntry  = (Float,Int)

sumfromto       :: Int -> Int-> Array Int Float -> Float
sumfromto i j p = sum [p!l | l<-[i..j]]

compObst :: Array Int Float -> Table ObstEntry ObstCoord -> ObstCoord
            -> ObstEntry
compObst p c (i,j) 
    | i > j     = (0.0,0)
    | i==j      = (p!i,i)
    | otherwise = addfst (minimum [(fst(findTable c (i,k-1)) 
                                  + fst(findTable c (k+1,j))  ,  k) 
                                  | k <- [i..j]] )
                         (sumfromto i j p)
                  where addfst (x,y) z = (x+z,y)

data BinTree a = EmptyBT
               | NodeBT a (BinTree a) (BinTree a) deriving Show

solObst :: Array Int Int -> Table ObstEntry ObstCoord -> ObstCoord
           -> BinTree Int
solObst keys c (i,j)
        | i > j     = EmptyBT
        | i == j    = NodeBT key EmptyBT EmptyBT
        | otherwise = NodeBT key (solObst keys c (i,k-1))
                                 (solObst keys c (k+1,j))
        where (_,k) = findTable c (i,j)
              key   = keys ! k

bndsObst   :: Int -> ((Int,Int),(Int,Int))
-- these range should be ((1,n),(1,n)) but in compObst 
-- indices (i,k-1) and (k+1,j) are needed i<= k <= j
-- adding a supplementary a row and column simplifies testing for 
-- the boundary conditions
bndsObst n = ((1,0),(n+1,n))

obst            :: [Int] -> [Float]  -> (BinTree Int,Float)
obst keys ps    = (solObst keysA t (1,n) , fst (findTable t (1,n)))
    where n = length ps
          keysA = listArray (1,n) keys
          psA   = listArray (1,n) ps
          t = dynamic (compObst psA) (bndsObst n) 

--Example in book

main = obst [   1,   3,   4,   8,  10,  11,  15]
            [0.22,0.18,0.20,0.05,0.25,0.02,0.08]

{- Examples of evaluations and results 
? main
(NodeBT 4 (NodeBT 1 EmptyBT (NodeBT 3 EmptyBT EmptyBT)) (NodeBT 10 (NodeBT 8 EmptyBT EmptyBT) (NodeBT 15 (NodeBT 11 EmptyBT EmptyBT) EmptyBT)), 2.15)
-- in pretty-printed form
(NodeBT 4 (NodeBT 1 EmptyBT
                    (NodeBT 3 EmptyBT EmptyBT))
          (NodeBT 10 (NodeBT 8 EmptyBT EmptyBT)
                     (NodeBT 15 (NodeBT 11 EmptyBT EmptyBT)
                                EmptyBT)),
 2.15)
-}
