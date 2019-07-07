
-- f n = 37+n
-- f True = 34

-- g 0 = 37
-- g n = True

-- h x
	-- | x>0 = True
	-- | otherwise = 37
	
k x = 34
k 0 = 35

f :: [a] -> [b] -> a -> b
f xs (y:ys) x = y

h x = f x x