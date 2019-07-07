import Prelude hiding (elem)

-- Ej 8: Give a definition of a function
-- doubleAll :: [Int] -> [Int]
-- which doubles all the elements of a list of integers.

doubleAll :: [Int] -> [Int]
doubleAll list = [ x*2 | x <- list]

{-  
	Ej 10: Define the function
	divisors :: Int -> [Int]
	which returns the list of divisors of a positive integer (and the empty list for other
	inputs). For instance,
	divisors 12 -+ [1,2,3,4,6,12]
	A prime number n is a number whose only divisors are 1 and n. Using divisors
	or otherwise define a function
	isprime :: Int -> Bool
	which checks whether or not a positive integer is prime (and returns False if its
	input is not a positive integer).					
-}

divisors :: Int -> [Int]
divisors n
	| n < 1 = []
	| otherwise = [ x | x <- [1 .. n], mod n x == 0]
	
isprime :: Int -> Bool
isprime n
	| n < 1 = False
	| otherwise = divisors n == [1, n]
	
{-	
	Ej 11: Define the function
	matches :: Int -> [Int] -> [Int]
	which picks out all occurrences of an integer n in a list. For instance,
	matches 1 [1,2,1,4,5,1] -> [1,1,1]
	matches 1 [2,3,4,6] []
	Using matches or otherwise, define a function
	elem :: Int -> [Int] -> Bool
	which is True if the Int is an element of the list, and False otherwise. For the
	examples above, we have
	elem 1 [1,2,1,4,5,1] --> True
	elem 1 [2,3,4,6] --> False
	Since elem is a prelude function, you need to hide it as described on page 41
-}

matches :: Int -> [Int] -> [Int]
matches n lista = [ n | x <- lista, x == n]

elem :: Int -> [Int] -> Bool
elem n lista = matches n lista /= []


type Person = String
type Book = String

type Database = [ (Person , Book) ]

exampleBase :: Database
exampleBase = [ ("Alice" , "Tintin") , ("Anna" , " Little Women") ,
				("Alice" , "Asterix") , ("Rory" , "Tintin") ]
				
borrowers :: Database -> Book -> [Person]
borrowers db book = [ p | (p,b) <- db, b == book ]

borrowed :: Database -> Book -> Bool
borrowed db book = borrowers db book /= []

numBorrowed :: Database -> Person -> Int
numBorrowed db = length . borrowedPerson db
		where
			borrowedPerson :: Database -> Person -> [Book]
			borrowedPerson db person = [ b | (p,b) <- db, p == person ]
