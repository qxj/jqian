module Set (Set,emptySet,setEmpty,inSet,addSet,delSet) where

import List

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

{--- List implementation ---}
newtype Set a = St [a]

emptySet  :: Set a       
setEmpty  :: Set a -> Bool            

instance (Show a) => Show (Set a) where
    showsPrec _ (St s) str = showSet s str

emptySet = St []

setEmpty (St []) = True
setEmpty _       = False

inSet x (St xs) = elem x xs

{-- unordered list --
inSet     :: (Eq a) => a -> Set a -> Bool  
addSet    :: (Eq a) => a -> Set a -> Set a 
delSet    :: (Eq a) => a -> Set a -> Set a 

{- with duplicates implementation -

addSet x (St a) = St (x:a)

delSet x (St xs) = St (filter (/= x) xs)

 end of unordered list with duplicates implementation -}

{- without duplicates implementation -} 

addSet x s@(St xs) | inSet x s  = s
                   | otherwise  = St (x:xs)

delSet x (St s) = St (delete x s)

{- end of without duplicates implementation -}

 end of unordered list  --}


{-  ordered without duplicates -}
inSet  :: (Ord a) => a -> Set a -> Bool  
addSet :: (Ord a) => a -> Set a -> Set a 
delSet :: (Ord a) => a -> Set a -> Set a 


inSet x (St s) = elem x (takeWhile (<= x) s)

addSet x (St s) = St (add x s)
    where add x []                   = [x]                
          add x s@(y:ys)| (x>y)      = y : (add x ys)
                        | (x<y)      = x : s
                        | otherwise  = s

delSet x (St s) = St (del x s)
    where del x []                   = []
          del x s@(y:ys)| (x>y)      = y : (del x ys)
                        | (x<y)      = s
                        | otherwise  = ys

{- end of ordered without duplicates -}
{--- end of List implementation ---}

{---  sets as binary number  ---

newtype Set = St Int

emptySet  :: Set        
setEmpty  :: Set -> Bool            
inSet     :: Int -> Set -> Bool
addSet    :: Int -> Set -> Set 
delSet    :: Int -> Set -> Set 

instance  Show Set where
    showsPrec _  s str = showSet (set2List s) str

emptySet = St 0

setEmpty (St n) = n==0

inSet i (St s)
    | (i>=0) && (i<=maxSet) = odd (s `div` (2^i))
    | otherwise             = error ("inEnumset:illegal element =" ++
                                     show i)

addSet i (St s)
    | (i>=0) && (i<=maxSet) = St (d'*e+m)
    | otherwise             = error ("insertEnumset:illegal element =" ++
                                     show i)
    where (d,m) = divMod s e
          e  = 2^i
          d' = if odd d then d else d+1

delSet i (St s)
    | (i>=0) && (i<=maxSet) = St (d'*e+m)
    | otherwise             = error ("delEnumset:illegal element =" ++
                                     show i)
    where (d,m) = divMod s e
          e = 2^i
          d' = if odd d then d-1 else d

set2List (St s) = s2l s 0
    where s2l 0 _             = []
          s2l n i | odd n     = i : s2l (n `div` 2) (i+1)
                  | otherwise = s2l (n `div` 2) (i+1)

maxSet = truncate (logBase 2 (fromInt (maxBound::Int))) - 1

---  end of sets as binary number  ---} 

