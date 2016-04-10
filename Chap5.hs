-- Empty module to serve as the default current module.
module Chap5 where

pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors n = [x | x <- [1..n], n `mod` x == 0]

notLast [] = []
notLast xs = take ((length xs) - 1) xs

perfects n = [x | x <- [1..n], sum (notLast (factors x)) == x]


scalarproduct xs ys = sum [a*b | (a,b) <- zip xs ys] 