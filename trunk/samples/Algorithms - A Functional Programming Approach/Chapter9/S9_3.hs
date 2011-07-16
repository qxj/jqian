module S9_3 where

import Dynamic

type CmmCoord   = (Int,Int)
type CmmEntry   = (Int,Int)

compCmm :: [Int] -> Table CmmEntry CmmCoord -> CmmCoord -> CmmEntry
compCmm d t (i,j) 
    | (i==j)    = (0,i)
    | otherwise = minimum [(fst(findTable t (i,k)) 
                            + fst(findTable t (k+1,j)) 
                            + d!!(i-1) * d!!k * d!!j  ,  k) 
                           | k <- [i..j-1]]

solCmm :: Table CmmEntry CmmCoord -> CmmCoord -> ShowS
solCmm t (i,j) str = 
     let (_,k) = findTable t (i,j)
     in if i==j 
        then showChar 'A' (shows i str)
        else showChar '('
              (solCmm t (i,k)
               (showChar ','
                (solCmm t (k+1,j)
                 (showChar ')' str))))
                        

bndsCmm   :: Int -> ((Int,Int),(Int,Int))
bndsCmm n = ((1,1),(n,n)) 

cmm   :: [Int] -> (String , Int)
cmm p = (solCmm t (1,n) "" , fst (findTable t (1,n)))
    where n = (length p) - 1
          t = dynamic (compCmm p) (bndsCmm n)

ex = [30,1,40,10,25]    -- our example

{- Examples of evaluations and results 

? cmm ex
("(A1,((A2,A3),A4))", 1400)

-}
