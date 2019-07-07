-- Ej 7: Using the addition function over the natural numbers, give a recursive definition
-- of multiplication of natural numbers.

multRec :: Int -> Int -> Int
multRec n m
	| m < 0 || n < 0 = error "Los numeros deben ser naturales"
	| m == 0 || n == 0 = 0
	| m == 1 = n
	| n == 1 = m
	| m > 1 = n + multRec n (m-1)

-- Ej 8: The integer square root of a positive integer n is the largest integer whose square
-- is less than or equal to n. For instance, the integer square roots of 15 and 16 are
-- 3 and 4, respectively. Give a primitive recursive definition of this function.

integerSquareRoot :: Int -> Int
integerSquareRoot n
	| n < 0 = error "El numero debe ser un natural"
	| n == 0 = 0
	| otherwise = integerSquareRootRec n n
	
integerSquareRootRec :: Int -> Int -> Int
integerSquareRootRec n m
	| n^2 <= m = n
	| otherwise = integerSquareRootRec (n-1) m

{- 	Ej 9: Given a function f of type Int -> Int give a recursive definition of a function
	of type Int -> Int which on input n returns the maximum of the values f 0,
	f 1, . . . , f n. You might find the max function defined in Section 3.4 useful.
	To test this function, add to your script a definition of some values of f thus:
	
	f 0 = 0
	f 1 = 44
	f 2 = 17
	f _ = 0
	
	and so on; then test your function at various values. -}

maxFun :: Int -> Int
maxFun n
	| n < 0 = error "El numero debe ser un natural"
	| otherwise = maxFunRec (n-1) (f n)
	
maxFunRec :: Int -> Int -> Int
maxFunRec n max
	| n == -1 = max
	| (f n) > max = maxFunRec (n-1) (f n)
	| otherwise = maxFunRec (n-1) max

f :: Int -> Int
f 0 = 3
f 1 = 44
f 2 = 17
f 3 = 0
f 4 = 55
f 5 = 33

-- Ej 10: Given a function f of type Int -> Int give a recursive definition of a function
-- of type Int -> Bool which on input n returns True if one or more of the values
-- f 0, f 1, . . . , f n is zero and False otherwise.

anyZero :: Int -> Bool
anyZero n
	| n < 0 = error "El numero debe ser un natural"
	| n == 0 && (f n /= 0) = False
	| f n == 0 = True
	| otherwise = anyZero (n-1)
	
-- Ej 13: Give a recursive definition of a function to find the highest common factor of
-- two positive integers.

highestCommonFactor :: Int -> Int -> Int
highestCommonFactor n m
	| n < 0 || m < 0 = error "Los numeros deben ser naturales" 
	| otherwise = highestCommonFactorRec n m (div (max n m) 2)
	
highestCommonFactorRec :: Int -> Int -> Int -> Int
highestCommonFactorRec n m fact
	| fact == 0 = 1
	| (mod n fact == 0) && (mod m fact == 0) = fact
	| otherwise = highestCommonFactorRec n m (fact-1)

{- 	Ej 14: Suppose we have to raise 2 to the power n. If n is even, 2*m say, then
	2^n = 2^(2*m) = (2^m)^2
	If n is odd, 2*m+l say, then
	2^n = 2^(2*m+1) = (2^m)^2*2
	Give a recursive function to compute 2^n which uses these insights.           -}

pow2n :: Integer -> Integer
pow2n n
	| n < 0 = error "La potencia debe ser un natural"
	| n == 0 = 1
	| mod n 2 == 0 = pow2n (div n 2) ^ 2
	| otherwise = pow2n (div n 2) ^ 2 * 2
	