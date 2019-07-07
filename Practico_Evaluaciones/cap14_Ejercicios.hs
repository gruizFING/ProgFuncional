
data Season = Spring | Summer | Autumn | Winter
	deriving (Eq,Ord,Enum,Show,Read)
	
data Month = January | February | March | April | May | June | July | August | September | October | November | December
	deriving (Eq,Ord,Enum,Show,Read)
	
whichSeason :: Month -> Season
whichSeason m
	| elem m [January .. March] = Summer
	| elem m [April .. June] = Autumn
	| elem m [July .. September] = Winter
	| elem m [October .. December] = Spring

data Shape = Circle Float | Rectangle Float Float
	deriving (Eq,Ord,Show,Read)
	
	
area :: Shape -> Float
area (Circle r ) = pi*r*r
area (Rectangle h w) = h*w

perimeter :: Shape -> Float
perimeter (Circle r) = pi*2*r
perimeter (Rectangle h w) = 2 * (h + w)

data NTree = NilT |
			 Node Int NTree NTree
		deriving (Eq,Ord,Show,Read)
			 
sumTree,depth :: NTree -> Int
sumTree NilT = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth NilT = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2 )
			 
leftSubTree, rigthSubTree :: NTree -> NTree

leftSubTree NilT = NilT
leftSubTree (Node n left right) = left

rigthSubTree NilT = NilT
rigthSubTree (Node n left right) = right

memberNTree :: Int -> NTree -> Bool
memberNTree _ NilT = False
memberNTree n (Node e l r)
	| n == e = True
	| otherwise = memberNTree n l || memberNTree n r
	
maxTree, minTree :: NTree -> Int
maxTree NilT = error "Arbol vacio"
maxTree (Node n l r)
	| l == NilT && r == NilT = n
	| l == NilT = max n (maxTree r)
	| r == NilT = max n (maxTree l)
	| otherwise = max n (max (maxTree l) (maxTree r))
	
minTree NilT = error "Arbol vacio"
minTree (Node n l r)
	| l == NilT && r == NilT = n
	| l == NilT = min n (minTree r)
	| r == NilT = min n (minTree l)
	| otherwise = min n (min (minTree l) (minTree r))
	
reflect :: NTree -> NTree
reflect NilT = NilT
reflect (Node n l r) = (Node n (reflect r) (reflect l))

twiceReflect :: NTree -> NTree
twiceReflect = reflect . reflect

collapse :: NTree -> [Int]
collapse NilT = []
collapse (Node n l r)
	| l == NilT = n : collapse r
	| otherwise = collapse l ++ (n : collapse r)
	

	


