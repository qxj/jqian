-- BACKTRACKING FRAMEWORK

module Backtracking where

--import "../haskell/ListOps.hs"
(\\) xs1 xs2 = foldl del xs1 xs2
       where []     `del`  _ = []
             (x:xs) `del`  y
                 | x == y    = xs
                 | otherwise = x : (xs `del` y)


flatten = foldr (++) []
---------------------------------------------------
--import "../adt/Stack.hs"

emptyStack :: Stack a
stackEmpty :: Stack a -> Bool
push       :: a -> Stack a -> Stack a
pop        :: Stack a -> Stack a
top        :: Stack a -> a

type Stack a  = [a]

emptyStack    = []

stackEmpty [] = True
stackEmpty _  = False

push x xs     = x:xs

pop (_:xs)    = xs
top (x:_)     = x
----------------------------------------------------
--import Heap

emptyHeap:: (Heap a)
heapEmpty:: (Heap a) -> Bool
findHeap :: (Ord a) => Int -> (Heap a) -> a
insHeap  :: (Ord a) => (Int,a) -> (Heap a) -> (Heap a)
delHeap  :: (Ord a) => Int -> (Heap a) -> (a,(Heap a))
pdown    :: (Ord a) => (a , (Heap a)) -> (Heap a)

-- IMPLEMENTATION

data (Ord a) => Heap a = Node a (Heap a) (Heap a) | Empty
    deriving Show

emptyHeap       = Empty

heapEmpty Empty = True
heapEmpty _     = False

findHeap n (Node v lf rt) 
     | (n==1)             = v
     | ((n `mod` 2) == 0) = findHeap (n `div` 2) lf
     | otherwise          = findHeap (n `div` 2) rt

insHeap (n,k) Empty 
                = (Node  k Empty Empty)

insHeap (n,k) (Node v lf rt) 

     | v < k    = if ((n `mod` 2) == 0)
                  then Node v (insHeap ((n `div` 2),k) lf) rt 
                  else Node v lf (insHeap ((n `div` 2),k) rt)   

     | otherwise= if ((n `mod` 2) == 0)
                  then Node k (insHeap ((n `div` 2),v) lf) rt 
                  else Node k lf (insHeap ((n `div` 2),v) rt) 

delHeap 1  (Node v Empty Empty) 
                          = (v,Empty)
delHeap k  (Node v lf rt) 
       | (k `rem` 2 == 0) = let (v',rest) = (delHeap (k `div` 2) lf)
                            in
                              (v', (Node v rest  rt))
       | otherwise        = let (v',rest) = (delHeap (k `div` 2) rt)
                            in 
                              (v', (Node v lf rest))

pdown (v , Empty)     = Empty
pdown (v , (Node _ Empty Empty)) 
                      = (Node v Empty Empty)
pdown (v , (Node _ (Node a lf rt) Empty)) 
          | a < v     = (Node a (Node v lf rt) Empty)
          | otherwise = (Node v (Node a lf rt) Empty)
pdown (v , (Node _ n1@(Node a _ _) n2@(Node b _ _))) 
          | a<b       = if v < a
                        then (Node v n1 n2)
                        else (Node a (pdown (v , n1)) n2)
          | otherwise = if v < b
                        then (Node v n1 n2)
                        else (Node b n1 (pdown (v , n2) ))


------------------------------------------------------------------------
--import "../adt/Priqueue.hs"
-- INTERFACE

emptyPQ :: PQueue a 
pqEmpty :: PQueue a -> Bool 
enPQ    :: (Ord a) => a -> PQueue a -> PQueue a 
dePQ    :: (Ord a) => PQueue a -> PQueue a
frontPQ :: (Ord a) => PQueue a -> a

-- HEAP IMPLEMENTATION

-- include Heap.hs when loading
type PQueue a     = (Int,Heap a)

emptyPQ           = (0,emptyHeap)

pqEmpty (_,t) 
    | heapEmpty t = True
    | otherwise   = False

enPQ k (s,t)      = (s+1,insHeap ((s+1),k) t)

frontPQ (_,t)     = findHeap 1 t

dePQ (s,t)        = (s-1,pdown (k,t'))
                    where 
                      (k,t') = delHeap s t  

-- BACKTRACKING

-- DIFFS : 1) Use implicit graph (fct SUCC INSTEAD OF G)
--         2) add goal function
--         3) no path accumulation
--         4) Assumes acyclic graph

searchDfs             :: (Eq node) => (node -> [node]) -> (node -> Bool) 
                          -> node -> [node]
searchDfs succ goal x = (search' (push x emptyStack) )
 where
   search' s  
    | (stackEmpty s)   = [] 
    | goal (top s)     = (top s):(search' (pop s))
    | otherwise        = let x = top s
                         in search' (foldr push (pop s) (succ x))
----------------------------------------------------------------------
---------------------------------------------------------------------
------------------------------------------------------------------
-- PRIORITY-FIRST FRAMEWORK

searchPfs             :: (Ord node) => (node -> [node]) -> (node -> Bool) 
                          -> node -> [node]
searchPfs succ goal x = (search' (enPQ x emptyPQ) )
 where
   search' q  
    | (pqEmpty q)      = [] 
    | goal (frontPQ q) = (frontPQ q):(search' (dePQ q))
    | otherwise        = let x = frontPQ q
                         in search' (foldr enPQ (dePQ q) (succ x))
--------------------------------------------------------------------
--Also counts how many nodes examined

-- searchPfs'             :: (Ord node) => (node -> [node]) -> (node -> Bool) 
--                           -> node -> [(node,Int)]
-- searchPfs' succ goal x = (search' (enPQ x emptyPQ) 0)
--  where
--    search' q  c
--     | (pqEmpty q)      = []
--     | goal (frontPQ q) = ((frontPQ q),c+1):(search' (dePQ q)(c+1))
--     | otherwise        = let x = frontPQ q
--                          in search' (foldr enPQ (dePQ q) (succ x)) (c+1)

