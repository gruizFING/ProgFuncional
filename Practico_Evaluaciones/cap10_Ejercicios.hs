-- Ej 3: Define a function composeList which composes a list of functions into a single
-- function. You should give the type of composeList, and explain why the
-- function has this type. What is the effect of your function on an empty list of
-- functions'?
double :: Int -> Int
double x = 2 * x

composeList :: [(a -> a)] -> (a -> a)
composeList [] = id
composeList (x:xs) = x . composeList xs

total :: (Int -> Int ) -> (Int -> Int)
total f = (\n -> foldr (+) 0 (map (f) [0 .. n])) 

f :: [Int] -> [Int]
f = filter (>0) . map (+1)

f2 :: [Int] -> [Int]
f2 = map (+1) . filter (>(-1))




