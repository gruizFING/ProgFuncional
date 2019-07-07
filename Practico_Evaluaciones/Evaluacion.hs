-- Evaluacion:
-- Pregunta 1
	-- - cambiar [1,2,3] (Node [-1,-2,-3] (Node [0] Tip Tip) (Node [4,8] Tip Tip))
	-- - Node [1,2,3] (cambiar [1,2,3] (Node [4,8] Tip Tip)) (cambiar [1,2,3] (Node [0] Tip Tip)
	-- - Node [1,2,3] (Node [1,2,3] (cambiar [1,2,3] Tip)) (Node [1,2,3] (cambiar [1,2,3] Tip)
	-- - Node [1,2,3] (Node [1,2,3] Tip) (Node [1,2,3] Tip) 
	
	-- B

-- Pregunta 2

-- tata = map (mod 0) [1,2,0,7,8] --> tata = [0,0,_,0,0]
-- Por lazy evaluation solo falla 
	--C

-- Pregunta 3

lolo g x = ys
	where ys = [x] ++ filter (curry g x) ys
	
	-- curry g x = g (fst x) (snd x)
	
	-- C ((b,b) -> Bool) -> b -> [b]
	
-- Pregunta 4
-- take 5 (lolo (uncurry (==)) 1)

	-- uncurry f (x,y) = f x y
	-- C
	
-- Pregunta 5
	-- D
	
-- Pregunta 6
	-- B
	
-- Pregunta 7
	-- foldl ope 0 [5,7,3] where ope n x = 10 * n + x
	{- 	foldl ope 0 [5,7,3] --> 5 --> 50 + 7 = 57 --> 570 + 3 = 573
		foldr ope 0 [5,7,3] --> 30 --> 100 -->  150
	-}
	
	-- D
-- Pregunta 8	
fibs ::[Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- C

-- Pregunta 9
pepe _ [] = []
pepe y (x:xs)
	| y == x = xs'
	| otherwise = y:xs'
	where
	xs' = pepe y xs
	
-- A

-- Pregunta 10
-- a) (pepe "hola")
-- b) (pepe "hola" "chau")
-- c) (pepe . head)
-- d) (map . pepe)	

-- B

-- Pregunta 11
{-	pepe 'a' "hol" -> a: pepe 'a' "ol" -- a : a : pepe 'a' "o" -- a : a : a : pepe 'a' "" -- "aaa"

-- B
-}
--Pregunta 12

foo :: (Num a) => Int -> a -> a
foo 0 = constante 1
foo (n+1) = combinar (*) (foo n)

constante x _ = x

combinar f g x = f x (g x)

-- foo 4 2 -- combinar (*) (foo 3) -- combinar (*) (combinar (*) (combinar (*) (combinar (*) 1)))
-- * 2 (* 2 (* 2 (* 2 1))) = 16

-- A

-- Pregunta 13
cc xs ys = [ x | x <- xs, y <- ys, x <= y]

-- let us = [1,1] in cc us us -- cc [1,1] [1,1] -- [x | x <- [1,1], y <- [1,1], x <= y ] -- [1,1,1,1]

-- C

-- Pregunta 14
f2 xs = [i | i <- xs, (i,j) <- map (\y -> (i,y)) xs, i > j]

-- f [1,2,3] -- [2,3,3]

-- B

-- Pregunta 15
e = (foldr (++) . map (:)) [1,2,3] [[filter even]]

-- C

-- Pregunta 16
posi a l
	| l1 /= [] = Just (head l1)
	| otherwise = Nothing
	where l1 = [i | (i,j) <- zip [0 .. (length l)] l , j == a]

-- B - (Eq a) => a -> [a] -> Maybe Int

-- Pregunta 17 B O.o

-- Pregunta 18
-- C

data Rose a = Fork a [Rose a]

member :: Eq a => a -> Rose a -> Bool
member a (Fork r ts) = r == a || any (member a) ts

-- Pregunta 19
-- (!!) (scanl mod 1 [0..4]) 1

	-- [1,_,_
	
-- A

-- Pregunta 20
f xss = \a -> let ope x y = x ++ [a] ++ y
				in foldr1 ope xss
				
-- f [[1,3],[2,4], [0,9,8]] 5 -- ope [1,3] (ope [2,4] (ope [0,9,8]))

-- C

-- Pregunta 21

data Tipo = Prim | Seg Tipo | Ter (Tipo, Tipo)
	deriving (Show)


