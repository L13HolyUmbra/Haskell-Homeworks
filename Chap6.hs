-- Empty module to serve as the default current module.
module Chap6 where

merge [] [] = []
merge [] (y:ys) = (y:ys)
merge (x:xs) [] = (x:xs)
merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
		    | otherwise = y:merge (x:xs) (ys)

lol x 0 = 1 
lol x y = x * (lol x (y-1))
			 
dand [] = True
dand (x:xs) = x && dand xs

dconcat[] = [] 
dconcat (x: xs) = x ++ dconcat xs

dreplicate 0 y = [] 
dreplicate x y = y: dreplicate (x-1) y 

dselect (xs) 0 = head(xs) 
dselect (xs) n = dselect (tail(xs)) (n-1)

dlast (x:xs) | null xs = x
	     | otherwise = dlast(xs)

dtake 0 [] = []
dtake 0 (x:xs) = [] 
dtake a [] = [] 
dtake a (x:xs) = x: dtake (a-1) xs 

take1 0 [] = [ ]
take1 0 (x : xs) =[]
take1 (n + 1) [ ] = [ ]
take1 (n + 1) (x : xs) = x : take1 n xs

dsum [] = 0
dsum (x:xs) = x + dsum xs

halve xs = splitAt (length xs `div` 2) xs
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs) where (ys, zs) = halve xs

delem a [] = False
delem a (x:xs) | a == x = True 
	       | otherwise = delem a (xs)

