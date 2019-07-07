
primes :: [Int]
primes = sieve [2 .. ]
sieve (x:xs) = x : sieve [ y | y <- xs , y `mod` x > 0 ]

memberOrd :: Ord a => [a] -> a -> Bool
memberOrd (x:xs) n
	| x<n = memberOrd xs n
	| x==n = True
	| otherwise = False
	
{- 	
	Ej 2: Using the list comprehension notation, define the Functions
	sublists, subsequences :: [a] -> [[a]]
	which return all the sublists and subsequences of a list. A sublist is obtained
	by omitting some of the elements of a list; a subsequence is a continuous block
	from a list. For instance, both [2,4] and [3,4] are sublists of [2,3,4], but
	only [3,4] is a subsequence.
-}

sublists, subsequences :: [a] -> [[a]]
sublists [] = []
sublists lista@(x:xs) = [x] : [ (x : (take i (drop m lista))) |  m <- [1 .. n], i <- [1 .. n-m] ] ++ sublists xs
	where
	n = (length lista)
	
subsequences [] = []
subsequences lista@(x:xs) = [x] : [ (x : (take i xs)) | i <- [1 .. n-1] ] ++ subsequences xs
	where
	n = (length lista)

{-
	Ej 22: Define the infinite lists of factorial and Fibonacci numbers,
	factorial = [1,1,2,6,24,l20,720, . . . ]
	fibonacci = [0,l,l,2,3,5,8,l3,2l, . . . ]
-}

factorialList :: [Integer]
factorialList = 1 : factorial [1 .. ]
factorial (x:xs) = x : factorial ((head xs * x) : tail xs)

fibonacciList :: [Integer]
fibonacciList = 0 : 1 : fibonacci [1 .. ]
fibonacci (x:xs) = x : fibonacci (head xs : (head xs + x) : tail xs)  


