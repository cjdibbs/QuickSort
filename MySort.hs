module MySort where

import Data.Array.ST

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p:xs) = (qsort lesser) ++ [p] ++ (qsort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

qsort2 :: Ord a => [a] -> [a]
qsort2 [] = []
qsort2 [a] = [a]
qsort2 [a,b] 
    | a <= b     = [a,b]
    | otherwise = [b,a] 
qsort2 [a,b,c]
    | a <= b && b <= c = [a,b,c]
    | a <= c && c <= b = [a,c,b]
    | b <= a && a <= c = [b,a,c] 
    | b <= c && c <= a = [b,c,a]
    | c <= a && a <= b = [c,a,b]
    | c <= b && b <= a = [c,b,a]
qsort2 (a:b:xs) 
    | a < b     = qsort2' a b xs
    | otherwise = qsort2' b a xs

qsort2' p1 p2 xs = (qsort2 lesser) ++ [p1] ++ (qsort2 middle) ++ [p2] ++ (qsort2 greater)
    where
        lesser = filter (< p1) xs
        middle = filter (\x -> x >= p1 && x < p2) xs 
        greater = filter (>= p2) xs