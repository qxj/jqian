import ST

-- TESTING MUTABLE ARRAYS


--1. Histogram with simple recursion

histogram :: Ix i => i -> i -> [i] -> Array i Int  
histogram lower upper l 
    =  _runST(
       newArray (lower,upper) 0 `thenST` \a ->
       (updateHist a l) `thenST` \_ -> 
       freezeArray a
       )

updateHist :: (Ix i) => (_MutableArray s i Int) -> [i] -> ST s () 
updateHist a [] = returnST ()
updateHist a (x:xs) = readArray a x `thenST` \v ->
                      writeArray a x (v+1) `thenST` \_ ->
                      updateHist a xs


--2. Histogram with mapST

{-
histogram :: Ix i => i -> i -> [i] -> Array i Int  
histogram lower upper l 
    =  _runST(
       newArray (lower,upper) 0 `thenST` \a ->
       (mapST (updateSlot a) l) `thenST` \_ -> 
       freezeArray a
       )

updateSlot :: (Ix i) => (_MutableArray s i Int) -> i -> ST s ()
updateSlot a x = readArray a x `thenST` \v ->
                 writeArray a x (v+1)
-}
-- test

main _ = 
        [AppendChan stdout (show
        (histogram (1::Int) 5 [3,2,1,2,4,5,4,3,2,1,2,1,2] )) ]

