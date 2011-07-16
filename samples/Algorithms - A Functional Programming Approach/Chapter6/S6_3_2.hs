module S6_3_2 where
-- Insertion Sort

-- naive implementation

insert :: (Ord a) => a -> [a] -> [a]
insert x xs = takeWhile (<= x) xs ++ [x] ++ dropWhile (<=x) xs

isort        :: (Ord a) => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs) 

-- more efficient implementation

isort' xs = foldr insert [] xs 

insert' key []                   = [key]
insert' key l@(x:xs) | key <=x   = key : l
                     | otherwise = x : (insert key xs)

isort'' xs = foldr insert' [] xs

ex = [3,1,4,1,5,9,2,8]

{-  Examples of evaluations and results 
    (with number of reductions given by hugs +s)

? isort ex
[1, 1, 2, 3, 4, 5, 8, 9]
(351 reductions, 591 cells)
? isort' ex
[1, 1, 2, 3, 4, 5, 8, 9]
(343 reductions, 562 cells)
? isort'' ex
[1, 1, 2, 3, 4, 5, 8, 9]
(343 reductions, 562 cells)

-}
