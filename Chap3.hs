-- Empty module to serve as the default current module.
module Chap3 where

second xs = head (tail xs) 

swap (x,y) = (y,x) 
pair x y = (x,y) 
double x = x * 2 
palindrome xs = reverse xs == xs
twice f x = f (f x)