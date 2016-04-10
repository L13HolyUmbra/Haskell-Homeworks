-- Empty module to serve as the default current module.
module Chap4 where

halve xs = splitAt (length xs `div` 2) xs

safetail1 xs = if null xs then [] else tail xs

safetail2 xs | null xs = []
	     | otherwise = tail xs 


safetail3 [] = []
safetail3 (_ :xs) = xs
