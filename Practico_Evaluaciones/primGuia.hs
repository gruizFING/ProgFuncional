mas :: Int -> Int -> Int
mas m n = m + n

--suma_par m n = (even . mas m) n

suma_par m = even . (+) m


remainder :: Int -> Int -> Int
remainder m n
	| m<n = m
	| otherwise = remainder (m-n) n

divide :: Int -> Int -> Int
divide m n
	| m<n = 0
	| otherwise = 1 + divide (m-n) n

	
f :: Int -> Int
f n = 3 * n

g1 :: Int -> Int
g1 n = (succ . f) n

g2 :: Int -> Int
g2 n = (f . succ) n

{-
f2 :: (Int -> Int) -> (Int -> Int)
f2 = id

f3 :: Int -> Int -> Int -> Int
f3 1 2 3 = 4

f4 :: Int -> (Int -> Int) -> Int
f4 = id

f5 :: (Int -> Int, Int) -> Int
f5 = id

f6 :: (Int -> Int) -> (Int, Int)	
f6 = id	
-}
double :: Int -> Int
double n = 2 * n

iter :: Int -> (a -> a) -> (a -> a)
iter n f = foldr ( . ) id (replicate n f)

h :: Char -> Bool
h = (\c -> (not . elem c) [' ', '\n', '\t'])

total :: (Int -> Int) -> (Int -> Int)
total f = (\n -> totalF f n 0 0)

totalF :: (Int -> Int) -> Int -> Int -> Int -> Int
totalF f n p accum
	| p == n+1 = accum
	| otherwise = totalF f n (p+1) (accum + f p)

	

