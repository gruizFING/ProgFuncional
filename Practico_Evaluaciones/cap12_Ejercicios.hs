
class Visible a where
	tostring :: a -> String
	size :: a -> Int

instance Visible Bool where
	tostring True = "True"
	tostring False = "False"
	size _ = 1
	
instance Visible a => Visible [a] where
	tostring = concat . map tostring
	size = foldr (+) 1 . map size
	
instance (Visible a, Visible b) => Visible (a, b) where
	tostring (x, y) = "(" ++ tostring x ++ "," ++ tostring y ++ ")"
	size (_,_) = 2
	

compare :: Visible a => a -> a -> Bool
compare x y = size x <= size y

instance Visible Char where
	tostring ch = [ch]
	size _ = 1
	
instance Visible Int where
	tostring x = show x
	size x = x


