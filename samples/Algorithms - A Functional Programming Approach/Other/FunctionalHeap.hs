module Heap(Heap,emptyHeap,heapEmpty,findHeap,insHeap,delHeap)
    where 

emptyHeap:: (Ord a) => Heap a
heapEmpty:: (Ord a) => Heap a -> Bool
findHeap :: (Ord a) => Int -> Heap a -> a
insHeap  :: (Ord a) => a -> Heap a -> Heap a
delHeap  :: (Ord a) => Int -> Heap a -> Heap a

-- IMPLEMENTATION
data (Ord a) => BinTree a = EmptyBT
                          | NodeBT a (BinTree a) (BinTree a)
    deriving Show

type Heap a = (Int, BinTree a)

emptyHeap = (0,EmptyBT)

heapEmpty (n,_) = n==0

findHeap i (n,t)
    | i>0 && i<=n = findTree i t
    | otherwise   = error "findHeap: element not found in Heap"

findTree :: (Ord a) => Int -> BinTree a -> a
findTree i (NodeBT v lf rt)
    | i==1      = v
    | even i    = findTree (i `div` 2) lf
    | otherwise = findTree (i `div` 2) rt

insHeap v (n,t) = (n+1, insTree v (n+1) t)

insTree :: (Ord a) => a -> Int -> BinTree a -> BinTree a
insTree v' 1 _ = NodeBT v' EmptyBT EmptyBT
insTree v' i (NodeBT v lf rt) 
    | even i    = NodeBT small (insTree big (i `div` 2) lf) rt
    | otherwise = NodeBT small lf (insTree big (i `div` 2) rt)
    where (small, big) = if (v<=v') then (v,v') else (v',v)

delHeap i h@(_,EmptyBT) = error ("delHeap: empty heap")
delHeap i h@(n,t@(NodeBT v lf rt))
    | i == n     = (n-1,t')
    | i>0 && i<n = (n-1,t'')
    | otherwise   = error ("delHeap: element "++ show i
                           ++ " not found in heap")
    where
    (v',t')   = delTreeLast n t -- get and delete last value
    (v'',t'') = delTree i v' t' -- delete i th value and replace with v'

delTreeLast :: (Ord a) => Int -> BinTree a -> (a,BinTree a)
delTreeLast i t@(NodeBT v lf rt)
    | i == 1    = (v,EmptyBT)
    | even i    = let (v',lf') = delTreeLast (i `div` 2) lf
                      in (v',NodeBT v lf' rt)
    | otherwise = let (v',rt') = delTreeLast (i `div` 2) rt
                      in (v',NodeBT v lf rt')

-- removes ith value and pushdown newv in this subtree
delTree :: (Ord a) => Int -> a -> BinTree a -> (a,BinTree a)
delTree i v' t@(NodeBT v lf rt) 
    | i == 1   = (v,pdown v' t)
    | even i   = let (v'',lf'') = delTree (i `div` 2) big lf 
                     in (v'',NodeBT small lf'' rt)
    | otherwise = let (v'',rt'') = delTree (i `div` 2) big rt
                      in (v'',NodeBT big lf rt'')
    where (small,big) = if (v<=v') then (v,v') else (v',v)

pdown :: (Ord a) => a -> BinTree a -> BinTree a 
pdown v'  EmptyBT   = EmptyBT
pdown v'  (NodeBT _ EmptyBT EmptyBT) 
    = (NodeBT v' EmptyBT EmptyBT)
pdown v'  (NodeBT _ (NodeBT v lf rt) EmptyBT) 
    | v < v'     = (NodeBT v (NodeBT v' lf rt) EmptyBT)
    | otherwise = (NodeBT v' (NodeBT v lf rt) EmptyBT)
pdown v' (NodeBT _ lf@(NodeBT vlf _ _) rt@(NodeBT vrt _ _)) 
    | vlf<vrt   = if v' < vlf
                  then (NodeBT v' lf rt)
                  else (NodeBT vlf (pdown v'  lf) rt)
    | otherwise = if v' < vrt
                  then (NodeBT v' lf rt)
                  else (NodeBT vrt lf (pdown v' rt))
