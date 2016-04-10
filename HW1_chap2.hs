module HW1_chap2 where

fact n = product [1..n]

double x = x+x

quadruple x = double(double x) 

-- Factorial of a positive integer:
factorial n = product [1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

n = a `div` length xs
	where 
	a = 10
	xs = [1,2,3,4,5]

last xs = head(reverse xs)
last2 xs = drop (length xs -1) xs
last3 xs = xs!!(length xs - 1)

init xs = take(length xs - 1) xs
init2 xs = reverse(tail(reverse xs))