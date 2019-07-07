-- Ej 2: How would you define the length function using map and sum?
import Prelude hiding (length)

length :: [a] -> Int
length xs = (sum . map f) xs
	where
	f :: a -> Int
	f a = 1
	
{-	
	Ej6:
	Give definitions of functions to take a list of integers, ns, and
	return the list consisting of thc squares of the integers in ns;
	return the sum of squares of items in ns;
	check whether all Items of the list are greater than zero.
-}

squaresList :: [Int] -> [Int]
squaresList ns = map square ns
	where
	square :: Int -> Int
	square n = n*n
	
sumSquaresList :: [Int] -> Int
sumSquaresList = sum . squaresList

allGreaterThanZero :: [Int] -> Bool
allGreaterThanZero = (and . map p)
	where
	p :: Int -> Bool
	p n = n > 0
	
{-
	Ej 8:
	State the type o f a d define a function twice which takes a function from integers
	to integers and an input integer, and whose output is the function applied to the
	input twice. For instance, with the double function and 7 as input, the result is
	28. What is the most general type of the function you have defined'?
-}

twice :: (Int -> Int) -> Int -> Int
twice f = f . f

double :: Int -> Int
double x = 2 * x

incre :: Int -> [Int] -> [Int]
incre = map . (+)

-- Ej 11: How would you define the sum of the squares of the natural numbers 1 to n using
-- map and foldr?

sumSquares1toN :: Int -> Int
sumSquares1toN n = foldr (+) 0 (map (^2) [1 .. n])

-- Ej 12: Define a function to give the sum of squares of the positive integers in a list of
-- integers.

sumSquaresPositives :: [Int] -> Int
sumSquaresPositives ns = foldr (+) 0 (map (^2) (filter (>0) ns))

mystery :: [a] -> [a]
mystery xs = foldr (++) [] (map sing xs)
	where
	sing x = [x]
	
-- Ej 17: Define a function
-- filterFirst :: (a -> Bool) -> [a] -> [a]
-- so that filterFirst p xs removes the first element of xs which does not have
-- the property p. Use this to give a version of returnLoan which returns only
-- one copy of a book. What does your function do on a list all of whose elements
-- have property p?

elimElem :: Eq a => a -> [a] -> [a]
elimElem x (y:ys)
	| x == y = ys
	| otherwise = y : (elimElem x ys)	
	
noCumplenP :: (a -> Bool) -> [a] -> [a]
noCumplenP p xs = filter (not . p) xs

filterFirst :: Eq a => (a -> Bool) -> [a] -> [a]
filterFirst p xs
	| (noCumplenP p xs) == [] = xs 
	| otherwise = elimElem (head (noCumplenP p xs)) xs

		
		
proximity :: a -> a -> Double
proximity x y = 0

q :: b -> Int
q p = 0
    where
    g = proximity p
	
		
		

	
