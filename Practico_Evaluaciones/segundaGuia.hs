-- Evalúe en Haskell
-- [(a,b) | a <- [1..3] , b <- [1..2]]
-- y deduzca cómo varían los valores de las variables a y b para generar la respuesta.

-- Res: [(1,1),(1,2),(2,1),(2,2),(3,1),(3,2)]

r :: [(Int,Int)]
r = [(a,b) | a <- [1..3] , b <- [1..2]]

-- Usando su deducción, evalúe en Haskell
-- [[(i,j) | i <- [1..4] ; j <- [i+1 .. 4]]
-- y verifique en Haskell.

-- Res: [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

-- r2 :: [(Int,Int)]
-- r2 = [(i,j) | i <- [1..4] , j <- [i+1 .. 4]]

-- numNegativos :: [Int] -> Int
-- numNegativos ns = length [ n | n <- ns, n < 0 ]

-- Dadas las siguientes reglas: (Ver ejercicio 17.8, pag.350)

      -- [x | x <- xs] = xs

      -- [f x | x <- xs] = map f xs

      -- [e | x <- xs, p x, ... ] = [e | x <- filter p xs, ... ]

      -- [e | x <- xs, y <- ys, ... ] = concat [[e | y <- ys, ...] | x <- xs]

-- Usarlas para transformar

        -- [x * x | x <- [1..100], even x]

        -- [(i,j) | i <- [1..100], j <- [i+1 .. 100]]

-- en expresiones que combinen funciones.

f1 :: [Int]
f1 = map (^2) [ x | x <- filter even [1 .. 100] ]

f1' :: [Int]
f1' = map (^2) (filter even [1 .. 100])

f2 :: [(Int,Int)]
f2 = concat [ [ (i,j) | j <- [i+1 .. 100] ] | i <- [1 .. 100] ]

f2' :: [(Int,Int)]
f2' = [(i,j) | i <- [1..100], j <- [i+1 .. 100]]

p :: [a] -> [b] -> Bool
p l1 l2 = length l1 == length l2

g :: [String] -> [(String, String)]
g xs = ys
	where ys = map (splitAt 3) xs
	
	
-- Definir ope y e para que se cumpla

        -- map f = foldr ope e

        -- unzip = foldr ope e

        -- reverse = foldr ope e

f3 :: (Int -> Int) -> [Int] -> [Int]
f3 f = map f

f3' :: (Int -> Int) -> [Int] -> [Int]
f3' f = foldr ((:) . f) []

f4 :: [(a,b)] -> ([a],[b])
f4 = foldr (\(a,b) (as,bs) -> (a:as,b:bs)) ([],[]) -- FRUTA!
