module S4_1_6 where

average xs = sum xs / fromInt (length xs)

average' xs = s / fromInt n
    where (s,n) = av xs
          av []     = (0,0)
          av (x:xs) = (x+s,n+1)
              where (s,n) = av xs

average'' xs = av' xs 0 0
    where av' []     s n = s / fromInt n
          av' (x:xs) s n = av' xs (x+s) (n+1)
