import Prelude hiding (product,and,or,reverse,unzip)
-- Ej 1: Give a pattern-matching definition of a function which returns the first integer
-- in a list plus one, if there is one, and returns zero otherwise.

firstPlus1 :: [Int] -> Int
firstPlus1 [] = 0
firstPlus1 (x:_) = x+1

-- Ej 2: Give a pattern-matching definition of a function which adds together the first
-- two integers in a list, if a list contains at least two elements; returns the head
-- element if the list contains one, and returns zero otherwise.

firstPlusSecond :: [Int] -> Int
firstPlusSecond [] = 0
firstPlusSecond (x:[]) = x
firstPlusSecond (x:y:_) = x + y

-- Ej 3: Give solutions to the previous two questions without using pattern matching

firstPlus1_2 :: [Int] -> Int
firstPlus1_2 l
	| null l = 0
	| otherwise = head l + 1
	
firstPlusSecond_2 :: [Int] -> Int
firstPlusSecond_2 l
	| null l = 0
	| length l == 1 = head l
	| otherwise = head l + (head . tail) l

-- Ej 4: Define the function
-- product :: [Int] -> Int
-- which gives the product of a list of integers, and returns 1 for an empty list; why
-- is this particular value chosen as the result for the empty list? R: Por el paso base

product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs

-- Ej 5: Define the functions
-- and, or :: [Bool] -> Bool
-- which give the conjunction and disjunction of a list of Booleans. For instance,
-- and [False, True] = False
-- or [False, True] = True
-- On an empty list and gives True and or gives False; explain the reason for
-- these choices.

and, or :: [Bool] -> Bool

and [] = True
and (x:xs) = x && and xs

or [] = False
or (x:xs) = x || or xs

-- Ej 6: Using primitive recursion over lists. define a function
-- elemNum :: Int -> [Int] -> Int
-- so that elemNum x xs returns the number of times that x occurs in the list xs.
-- Can you define elemNum without using primitive recursion, using list comprehensions
-- and built-in functions instead?

elemNum :: Int -> [Int] -> Int
elemNum _ [] = 0
elemNum x (y:ys)
	| x == y = 1 + elemNum x ys
	| otherwise = elemNum x ys
	
-- Ej 7: Define a function
-- unique :: [Int] -> [Int]
-- so that unique xs returns the list of elements of xs which occur exactly once.
-- For example, unique [4,2,1,3,2,3] is [4,1]. You might like to think of
-- two solutions to this problem: one using list comprehensions and the other not.

unique1, unique2 :: [Int] -> [Int]
unique1 xs = [ x | x <- xs, elemNum x xs == 1 ]

unique2 [] = []
unique2 (x:xs)
	| elemNum x xs == 0 = x : unique2 xs
	| otherwise = (unique2 . deleteElem x) xs
	where
		deleteElem :: Int -> [Int] -> [Int]
		deleteElem _ [] = []
		deleteElem x (y:ys)
			| x == y = deleteElem x ys
			| otherwise = y : deleteElem x ys
	
-- Ej8 : Give primitive recursive definitions of the prelude functions reverse and unzip.

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((a,b):xs) = (a:aS, b:bS)
	where
		(aS,bS) = unzip xs

-- By modifying the definition of the ins function we can change the behaviour of
-- the sort, iSort. Redefine ins in two difrerent ways so that
-- the list is sorted in descending order;
-- duplicates are removed from the list. For example.
-- iSort [2,1,4,1,2] = [1,2,4]
-- under this definition.

ins1, ins2, ins3 :: Int -> [Int] -> [Int]
ins1 x [] = [x]
ins1 x (y:ys)
	| x <= y = x:(y:ys)
	| otherwise = y : ins1 x ys
	
ins2 x [] = [x]
ins2 x (y:ys)
	| x >= y = x:(y:ys)
	| otherwise = y : ins2 x ys
	
ins3 x [] = [x]
ins3 x (y:ys)
	| elem x (y:ys) = (y:ys)
	| x <= y = x:(y:ys)
	| otherwise = y : ins3 x ys
	
iSort1, iSort2, iSort3 :: [Int] -> [Int]
iSort1 [] = []
iSort1 (x:xs) = ins1 x (iSort1 xs)

iSort2 [] = []
iSort2 (x:xs) = ins2 x (iSort2 xs)

iSort3 [] = []
iSort3 (x:xs) = ins3 x (iSort3 xs)

{- 
	Ej 18: One list is a sublist of another if the elements of the first occur in the second, in
	the same order. For instance, "ship" is a sublist of "Fish & Chips", but not
	of "hippies".
	A list is a subsequence of another if it occurs as a sequence of elements next
	to each other. For example. "Chip" is a subsequence of "Fish & Chips", but
	not of "Chin up".
	Define functions which decide whether one string is a sublist or a subsequence
	of another string.
-}

sublist, subsequence :: Eq a => [a] -> [a] -> Bool
sublist [] a = True
sublist (x:_) [] = False
sublist (x:xs) (y:ys)
	| x == y = sublist xs ys
	| otherwise = sublist (x:xs) ys

subsequence [] _ = True
subsequence (x:_) [] = False
subsequence (x:xs) (y:ys)
	| x == y = xs == take (length xs) ys
	| otherwise = subsequence (x:xs) ys

-- Ej 19: Define the function dropLine specified in the text.

type Line = [Word]
type Word = String

dropLine :: Int -> [Word] -> Line
dropLine len []  = []
dropLine len (w:ws)
	| length w <= len = restOfLine
	| otherwise = (w:ws)
	where
		newLen = len - (length w + 1)
		restOfLine = dropLine newLen ws
		







