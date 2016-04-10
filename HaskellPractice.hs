sum' :: Num a => [a] -> a
sum' = foldr (+) 0

-- non-recursive, using list comprehension
len1		::	[a] -> Int
len1 xs	=	sum[1 | _ <- xs]

-- recursive, with conditionals
len2		::	[a] -> Int
len2 xs	=	if null xs then 0 else 1 + len2 (tail xs)

-- guarded
len3		::	[a] -> Int
len3 xs	|	null xs	=	0
		|	otherwise	=	1 + len3 (tail xs)
	
-- pattern matching
len4		::	[a] -> Int
len4 []	=	0
len4 (x:xs)	=	1 + len4 xs

--factorials
--using product
fact1		::	Int -> Int
fact1 n	=	product [1..n]

--recursive, with conditionals
fact2 	::	Int -> Int
fact2 n	=	if n == 0 then 1 else n * fact2 (n - 1)

--with guarded equations
fact3		::	Int -> Int
fact3 n	|	n == 0 = 1
		|	otherwise = n * fact3 (n - 1)

--with patterns
fact4		::	Int -> Int
fact4 0	=	1
fact4 n	=	n * fact4 (n - 1)

--reverse a list using patterns
--reverse
rev1		::	[a] -> [a]
rev1 []	=	[]
rev1 (x:xs)	=	(rev1 xs) ++ [x]

--doubleAll's
--list comprehension
doubleAll1		::	Num a => [a] -> [a]
doubleAll1	xs	=	[2 * x | x <- xs]

--recursive



