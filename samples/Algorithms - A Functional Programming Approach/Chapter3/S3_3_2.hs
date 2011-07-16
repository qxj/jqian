module S3_3_2 where

average xs = sum xs / fromInt(length xs)

length' xs = lengthTR xs 0
    where lengthTR [] r     = r
          lengthTR (x:xs) r = strict (lengthTR xs) (r+1)

sum' xs = sumTR xs 0
    where sumTR [] r     = r
          sumTR (x:xs) r = strict (sumTR xs) (r+x)

average' xs = sum' xs / fromInt(length' xs)
