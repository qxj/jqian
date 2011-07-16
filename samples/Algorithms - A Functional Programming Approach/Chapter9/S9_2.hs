module S9_2 where

import Dynamic

bndsFib :: Int -> (Int,Int)
bndsFib n = (0,n)

compFib                :: Table Int Int -> Int -> Int
compFib t i | i<=1     = i
            |otherwise = findTable t (i-1) + findTable t (i-2)

fib :: Int -> Int
fib n = findTable t n
    where t = dynamic compFib (bndsFib n) 


fibs = 0:1:[(fibs!!(n-1)+fibs!!(n-2)) | n <-[2..]]

fib' n = fibs!!n

{- Examples of evaluations and results 
? fib 10
55
? fib' 10
55
? fibs
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296, 433494437, 701408733, 1134903170, 1836311903, -1323752223, 512559680,  {Interrupted!}
-- note the overflow of Int...
-}
