-- 4317743 4786543
-- German Ruiz, Federico Mujica
module Main (main) where

import Data.IntSet (IntSet,empty,fromList,toList,difference,intersection,union,toAscList,elems,member)
import qualified Data.IntSet as IntSet
import Char (ord,chr,digitToInt,intToDigit)
import Hugs.Observe

type Posicion = (Int,Int)
type Posiciones = [(Int,Int)]
type Digitos = IntSet
type Libres = [(Posicion,Digitos)]
type Ocupadas = [(Posicion,Int)]
type Sudoku = (Libres,Ocupadas)


newOcupadas :: Posicion -> Ocupadas
newOcupadas (8,8) = [((8,8),0)]
newOcupadas (x,8) = [((x,8),0)] ++ newOcupadas (x+1,0)
newOcupadas (x,y) = [((x,y),0)] ++ newOcupadas (x,y+1)

updateOcupadas :: Ocupadas -> Posicion -> Int -> Ocupadas
updateOcupads [] _ _ = error "Update de Ocupadas fallido, celda no existente"
updateOcupadas (ocup@((x1,y1),num):rest) (x2,y2) numNuevo
	| x1 == x2 && y1 == y2 = (((x1,y1),numNuevo):rest)
	| otherwise = ocup : (updateOcupadas rest (x2,y2) numNuevo)
	
toStringOcupadas :: Ocupadas -> String
toStringOcupadas [] = []
toStringOcupadas (((x,y),num):rest)
	| num == 0 && y == 8 = ".\n" ++ toStringOcupadas rest
	| num == 0 = ". " ++ toStringOcupadas rest
	| y == 8 = show num ++ "\n" ++ toStringOcupadas rest
	| otherwise = show num ++ " " ++ toStringOcupadas rest
	
	
updateLibres :: Libres -> Posicion -> Int -> Libres
updateLibres [] _ _ = []
updateLibres ((p,c):ls) pos num
	| p == pos = updateLibres ls pos num
	| mismoAmbito p pos = (p,difference c (fromList [num])) : updateLibres ls pos num
	| otherwise = (p,c) : updateLibres ls pos num
	
updateLibresPE :: Libres -> [Posicion] -> [Int] -> Libres
updateLibresPE [] _ _ = []
updateLibresPE ((p,c):ls) poss  nums
	| p > last poss = ((p,c):ls)
	| elem p poss = (p,fromList nums) : updateLibresPE ls poss nums
	| otherwise = (p,c) : updateLibresPE ls poss nums
																		
updateLibresCeldas :: Libres -> [Int] -> Libres -> Libres
updateLibresCeldas libres _ [] = libres
updateLibresCeldas ((p,c):ls) nums celdas
	| memberCelda p celdas = (p,difference c (fromList nums)) : updateLibresCeldas ls nums (deleteCelda p celdas)
	| otherwise = (p,c) : updateLibresCeldas ls nums celdas
	
memberCelda :: Posicion -> Libres -> Bool
memberCelda _ [] = False
memberCelda pos ((p,_):ls)
	| pos == p = True
	| otherwise = memberCelda pos ls
	
deleteCelda :: Posicion -> Libres -> Libres
deleteCelda _ [] = []
deleteCelda pos ((p,c):ls)
	| pos == p = ls
	| otherwise = (p,c) : deleteCelda pos ls
	
updateLibresE :: Libres->[Posicion] -> Int ->Libres
updateLibresE [] [] num  = []
updateLibresE ((p,c):ls) [] num  = ((p,c):ls)
updateLibresE ((p,c):ls) (x:xs) num  
									 | ((fst x) == fst (p) && snd x == snd (p)) =   (p,(difference c (fromList [num]))) : updateLibresE ls xs num
									 | otherwise  = (p,c) :  updateLibresE ls (x:xs) num
													
toStringLibres :: Libres -> String
toStringLibres ((pos,cands):[]) = show pos
toStringLibres ((pos,cands):ls) = show pos ++ "," ++ toStringLibres ls 

showCoord :: Ambito -> String
showCoord (Fila x)    = show x
showCoord (Columna x) = show x
showCoord (Region x)  = show (cambiarFormatoRegion x)
		 
		 
toString :: [Int] -> String
toString l = foldr (++) "" (map (show) l)

data Tactica = Unico Posicion Int | 
               Exclusivo Posicion Int Ambito | 
			   Descubiertos [Posicion] [Int] Libres | 
			   Escondidos [Posicion] [Int] |
			   Encerrado String Ambito Ambito Int [Posicion]
			   
instance Show Tactica where
	show (Unico pos num) = "Unico " ++ show pos ++ " '" ++ show num ++ "'"
	show (Exclusivo pos num (Fila _))    = "Exclusivo " ++ show pos ++ " '" ++ show num ++ "' " ++ "Fila"
	show (Exclusivo pos num (Columna _)) = "Exclusivo " ++ show pos ++ " '" ++ show num ++ "' " ++ "Columna"
	show (Exclusivo pos num (Region _))  = "Exclusivo " ++ show pos ++ " '" ++ show num ++ "' " ++ "Region"
	show (Descubiertos poss nums lib)  = "Descubiertos " ++ show poss ++ " \"" ++ toString nums ++ "\" [" ++ toStringLibres lib ++ "]"
	show (Escondidos poss nums)  = "Escondidos " ++ show poss ++ " \"" ++ toString nums ++ "\""
	show (Encerrado tactica region fila_col cand poss) = "Encerrado " ++ "\"" ++ tactica ++  "\" " ++ showCoord region ++ " " ++ showCoord fila_col ++ " '" ++ show cand ++ "' " ++ show  poss  
	
cambiarFormatoRegion :: Int->(Int,Int)
cambiarFormatoRegion x | x == 0 = (0,0) | x == 1 = (0,1) | x == 2 = (0,2) | x == 3 = (1,0) | x == 4 = (1,1) | x == 5 = (1,2) | x == 6 = (2,0) | x == 7 = (2,1)| x == 8 = (2,2)

	
data Ambito = Fila Int |
              Columna Int |
			  Region Int
			  deriving Show			  

ambitos  :: [Ambito]
ambitos = [Fila x | x<-[0 .. 8]] ++ [Columna x | x<-[0 .. 8]] ++ [Region x | x<-[0 .. 8]]

pares :: Libres -> [((Posicion,Digitos),(Posicion,Digitos))]
pares libres = [ ((pos1,cs1),(pos2,cs2)) | (pos1,cs1) <- libres, (pos2,cs2) <- libres, pos2 > pos1 ]

paresInts :: [Int] -> [(Int,Int)]
paresInts cands = [ (c1,c2) | c1 <- cands, c2 <- cands, c2 > c1 ]

ternas :: Libres -> [((Posicion,Digitos),(Posicion,Digitos),(Posicion,Digitos))]
ternas libres = [ ((pos1,cs1),(pos2,cs2),(pos3,cs3)) | 
					(pos1,cs1) <- libres,
					(pos2,cs2) <- libres,
					pos2 > pos1,
					(pos3,cs3) <- libres,
					pos3 > pos2
				]

obtenerCeldas :: Ambito -> Libres -> Libres
obtenerCeldas (Fila x) l = [li | li<-l , fst (fst li) == x]
obtenerCeldas (Columna x) l = [li | li<-l , snd (fst li) == x]
obtenerCeldas (Region x) l = [li | li<-l , getRegion (fst li) == x]
	
otras :: [Posicion]	-> Digitos -> Libres -> Libres
otras poss cands = filter (\(pos,cs) -> (not . elem pos) poss && not (IntSet.null (intersection cands cs)))

unicos :: Libres -> [Tactica]
unicos libres = [ Unico pos ((head . toList)cands) |
					-- obtengo una celda libre
					(pos,cands) <- libres,
					-- candidato unico
					IntSet.size cands == 1
				]
										 
exclusivos :: Libres -> [Tactica]
exclusivos libres = [ Exclusivo pos cand ambito |
						-- obtengo una celda libre
						(pos@(x,y),cands) <- libres,
						-- obtengo uno de los ambitos de la celda en el orden Fila, Columna y Region
						ambito <- [Fila x, Columna y, Region (getRegion pos)],
						-- obtengo las celdas de ese ambito
						let celdas =  obtenerCeldas ambito libres,
						-- obtengo cada candidato de cands
						cand <- toList cands,
						-- obtengo las restantes de la fila que contienen a cand como candidato
						let restantes = otras [pos] (fromList [cand]) celdas,
						-- restantes debe ser vacia
						null restantes
					]	

paresDescubiertos :: Libres -> [Tactica]      
paresDescubiertos  libres =	[ Descubiertos [p1,p2] (toAscList c1s) restantes |
								-- obtengo	 un ambito
								ambito <- ambitos,
                                -- obtengo las celdas de ese ambito
							    let celdas =  obtenerCeldas ambito libres,
								-- obtengo un par de celdas con sus candidatos
								((p1,c1s),(p2,c2s)) <- pares celdas,
								-- deben ser solo 2 candidatos
								IntSet.size c1s == 2, IntSet.size c2s == 2,
								-- los canditados deben ser iguales
								IntSet.null (difference c1s c2s),
                                -- obtengo las restantes del ámbito que contienen alguna del par
							    let restantes = otras [p1,p2] c1s celdas,
                                -- debe haber restantes
								not (null restantes) 
							]
							
paresEscondidos :: Libres -> [Tactica]      
paresEscondidos  libres =	[ Escondidos [p1,p2] [c1,c2] |
								-- obtengo	 un ambito
								ambito <- ambitos,
                                -- obtengo las celdas de ese ambito
							    let celdas =  obtenerCeldas ambito libres,
								-- obtengo un par de celdas con sus candidatos
								((p1,c1s),(p2,c2s)) <- pares celdas,
								-- al menos 1 de las celdas tiene mas de 2
								IntSet.size c1s > 2 || IntSet.size c2s > 2,
								-- obtengo la interseccion de los candidatos
								let interCands = intersection c1s c2s,
								-- obtengo pares de los candidatos en comun
								(c1,c2) <- paresInts (toAscList interCands),
                                -- obtengo las restantes del ámbito que contienen alguna del par (c1,c2)
							    let restantes = otras [p1,p2] (fromList [c1,c2]) celdas,
                                -- no debe haber restantes
								null restantes 
							]
							
ternasDescubiertas :: Libres -> [Tactica]      
ternasDescubiertas  libres =	[ Descubiertos [p1,p2,p3] (toAscList cs) restantes |
									-- obtengo	 un ambito
									ambito <- ambitos,
									-- obtengo las celdas de ese ambito
									let celdas =  obtenerCeldas ambito libres,
									-- obtengo una terna de celdas con sus candidatos
									((p1,c1s),(p2,c2s),(p3,c3s)) <- ternas celdas,
									-- hago la union de los candidatos de las tres celdas
									let cs = (union . union c1s) c2s c3s,
									-- deben ser tres candidatos
									IntSet.size cs == 3,
									-- obtengo las restantes del ámbito que contienen alguna de la terna
									let restantes = otras [p1,p2,p3] cs celdas,
									-- debe haber restantes
									not (null restantes) 
								]

--saco de los libres todas las celdas que se encuentran en el ambito								
quitarCeldasAmbito :: Ambito -> Libres -> Libres
quitarCeldasAmbito amb l = [li | li<-l , not (perteneceAAmbito (fst li) amb) ]

--devuelvo las celdas de la intercepcin de ambitos
interseccionAmbitos :: Ambito -> Ambito -> Libres -> Libres
interseccionAmbitos amb1 amb2 l = [li | li<-l , perteneceAAmbito (fst li) amb1 , perteneceAAmbito (fst li) amb2]

--true si la posicion pertenece al ambito
perteneceAAmbito :: Posicion -> Ambito -> Bool
perteneceAAmbito (f,c) (Fila x) = x == f
perteneceAAmbito (f,c) (Columna x) = x == c
perteneceAAmbito (f,c) (Region x) = getRegion (f,c) == x
								 
				
pertenecenFilaRegion :: Ambito -> Ambito -> Bool
pertenecenFilaRegion (Region r) (Fila f) | r < 3 = f<3
						         	     | r >2 && r < 6 = f > 2 && f <6
									     | r > 5 = f > 5
										 
pertenecenColumnaRegion :: Ambito -> Ambito -> Bool
pertenecenColumnaRegion (Region x) (Columna f)	| x == 0 || x == 3 || x == 6 = f<3
												| x == 1 || x == 4 || x == 7 = f > 2 && f <6
												| x == 2 || x == 5 || x == 8 = f > 5
										 									 
getEncerradosRF :: [(Ambito,Ambito)]
getEncerradosRF      =    [(r ,f ) | r  <-[Region x | x<-[0 .. 8]]       ,  f <-[Fila x | x<-[0..8]]  , pertenecenFilaRegion r  f] 

getEncerradosRC :: [(Ambito,Ambito)]
getEncerradosRC = [(r,f) | r <-[Region x | x<-[0 .. 8]]       ,  f<-[Columna x | x<-[0..8]]  , pertenecenColumnaRegion r  f] 

getPosElem :: Int -> Libres -> Posiciones
getPosElem cand l = [pos | (pos,candi)<-l, member cand candi]

eliminarCandidatos :: Int -> Libres -> Libres
eliminarCandidatos cand l = [li| li<-l, not (member cand (snd li))]
--Encerrado fila region

borrar :: Libres -> Libres
borrar [] = []
borrar (((f,c),cand):ls) = ((f,c),cand) : borrar (menos cand ls)

menos :: IntSet -> Libres -> Libres
menos cand [] = []
menos cand ((p,c):ls) = (p,difference c cand) : menos cand ls


encerradoFilaRegion :: Libres -> [Tactica]
encerradoFilaRegion  libres = [ Encerrado "Region-Fila" reg  fila  cand posiciones |								
									--Agrupo por region fila posible
									(reg ,fila ) <- getEncerradosRF,
									--Obtengo la celdas que contiene la interseccion de la region y la fila
									let rInterF = interseccionAmbitos reg fila libres,
									--Obtengo las celdas que pertenecen a la Region								
									let celdasRegion =  obtenerCeldas reg libres,
									--Obtengo R - F
									let rmenosF = quitarCeldasAmbito  fila celdasRegion,
									--obtengo la posiciones y candidatos
									(pos,candRintF) <- borrar  rInterF,
									--Analizo los candidatos un por uno
									cand <- elems candRintF,								
									--Obtengo las celdas de R-F que contienen el candidato
									let restantesRmenosF = otras [pos] (fromList[cand]) rmenosF,
									--Tiene que ser vacio el resultado
									null restantesRmenosF,
									--Obtengo la celdas de la fila
									let celdasFilas  =  obtenerCeldas fila libres,
									--Obtengo F - R
									let fMenosR = quitarCeldasAmbito reg celdasFilas,
									--Obtengo las celdas que tienen el mismo candidato en F -R
									let posiciones = getPosElem cand fMenosR,
									let restantesFmenosR = otras [pos] (fromList [cand]) fMenosR,
									--Tiene que haber por lo menos uno
									not (null restantesRmenosF)
								]
								
encerradoRegionFila :: Libres -> [Tactica]	
encerradoRegionFila libres = [ Encerrado "Fila-Region"  reg fila cand posiciones |	
									--Agrupo por region fila posible
									(reg,fila) <- getEncerradosRF, 							
									--Obtengo la celdas que contiene la interseccion de la region y la fila
									let rInterF = interseccionAmbitos reg fila libres,
									--Obtengo las celdas que pertenecen a la Region
									let celdasFila =  obtenerCeldas fila libres,
									--Obtengo F - R
									let fmenosR = quitarCeldasAmbito  reg celdasFila,
									--obtengo la posiciones y candidatos
									(pos,candRintF) <- borrar  rInterF,
									--Analizo los candidatos un por uno
									cand <- elems candRintF,
									--Obtengo las celdas de R-F que contienen el candidato
									let restantesFmenosR = otras [pos] (fromList[cand]) fmenosR,
									--Tiene que ser vacio el resultado
									null restantesFmenosR,
									--Obtengo la celdas de la fila
									let celdasRegion  =  obtenerCeldas reg libres,
									--Obtengo F - R
									let rMenosF = quitarCeldasAmbito fila celdasRegion,
									--Obtengo las celdas que tienen el mismo candidato en F -R
									let posiciones = getPosElem cand rMenosF,
									let restantesFmenosR = otras [pos] (fromList [cand]) rMenosF,
									--Tiene que haber por lo menos uno
									not (null restantesFmenosR)
								] 
														
encerradoRegionColumna :: Libres -> [Tactica]	
encerradoRegionColumna libres = [ Encerrado "Columna-Region" reg  col cand posiciones |	
									--Agrupo por region fila posible
									(reg,col) <- getEncerradosRC,
									--Obtengo la celdas que contiene la interseccion de la region y la fila
									let rInterC = interseccionAmbitos reg col libres,
									--Obtengo las celdas que pertenecen a la Region
									let celdasColumna =  obtenerCeldas col libres,
									--Obtengo C - R
									let cmenosR = quitarCeldasAmbito  reg celdasColumna,								
									--obtengo la posiciones y candidatos
									(pos,candRintF) <- borrar  rInterC,
									--Analizo los candidatos un por uno
									cand <- elems candRintF,
									--Obtengo las celdas de C-R que contienen el candidato
									let restantesCmenosR = otras [pos] (fromList[cand]) cmenosR,
									--Tiene que ser vacio el resultado
									null restantesCmenosR,
									--Obtengo la celdas de la fila
									let celdasRegion  =  obtenerCeldas reg libres,
									--Obtengo R - C
									let rMenosC = quitarCeldasAmbito col celdasRegion,
									--Obtengo las celdas que tienen el mismo candidato en R-C
									let posiciones = getPosElem cand rMenosC,
									let restantesRmenosC = otras [pos] (fromList [cand]) rMenosC,
									--Tiene que haber por lo menos uno
									not (null restantesRmenosC)
								]
								
encerradoColumnaRegion :: Libres -> [Tactica]
encerradoColumnaRegion libres = [ Encerrado "Region-Columna" reg col cand posiciones |	
									--Agrupo por region fila posible
									(reg,col) <- getEncerradosRC,
									--Obtengo la celdas que contiene la interseccion de la region y la fila
									let rInterC = interseccionAmbitos reg col libres,
									--Obtengo las celdas que pertenecen a la Region
									let celdasRegion  =  obtenerCeldas reg libres,
									let rMenosC = quitarCeldasAmbito col celdasRegion,
									--obtengo la posiciones y candidatos
									(pos,candRintF) <- borrar  rInterC,
									--Analizo los candidatos un por uno
									cand <- elems candRintF,
									let posiciones = getPosElem cand rInterC,
									let restantesRmenosC = otras [pos] (fromList[cand]) rMenosC,
									--Tiene que ser vacio el resultado
									null restantesRmenosC,
									--Obtengo la celdas de la fila
									let celdasColumna =  obtenerCeldas col libres,
									--Obtengo C - R
									let cmenosR = quitarCeldasAmbito  reg celdasColumna,
									let posiciones = getPosElem cand cmenosR,
									--Obtengo las celdas que tienen el mismo candidato en C-R
									let restantesRmenosC = otras [pos] (fromList [cand]) cmenosR,
									--Tiene que haber por lo menos uno
									not (null restantesRmenosC)
								]
								
	
-- Carga la estructura correspondiente al sudoku con el string leido de la entrada
-- Tambien se inicializan los candidatos de cada celda libre en funcion de los ocupados
cargarSudoku :: String -> Posicion -> Sudoku -> Sudoku
cargarSudoku _ (8,9) s = inicializarCandidatos s
cargarSudoku (e:es) (x,y) (l,o)
	| e == '0'  = cargarSudoku es (x,y+1) (l ++ [((x,y),fromList [1..9])], o)
	| e == '\n' = cargarSudoku es (x+1,0) (l,o)
	| otherwise = cargarSudoku es (x,y+1) (l, updateOcupadas o (x,y) (digitToInt e))
	
-- En funcion de las celdas ocupadas se inicializan los candidatos de las libres
-- Solo se usa cuando se carga la estructura por primera vez
inicializarCandidatos :: Sudoku -> Sudoku
inicializarCandidatos (libres, ocupadas) = ([ (pos,candNuevo) | 
												(pos,cand) <- libres,
												let dig = getDigitosOcupadas pos ocupadas,
												let candNuevo = difference cand dig
											]
											, ocupadas)
											
getDigitosOcupadas :: Posicion -> Ocupadas -> Digitos
getDigitosOcupadas (x,y) ocup = fromList 	[ dig | 
												(posOc,dig) <- ocup,
												mismoAmbito (x,y) posOc
											]

mismoAmbito :: Posicion -> Posicion -> Bool
mismoAmbito (x1,y1) (x2,y2)
	| x1 == x2 = True	-- Misma Fila
	| y1 == y2 = True	-- Misma Columna
	| r1 == r2 = True	-- Misma Region
	| otherwise = False
		where
		r1 = getRegion (x1,y1)
		r2 = getRegion (x2,y2)
		
getRegion :: Posicion -> Int
getRegion (x,y)
	| x >= 0 && x < 9 && y >= 0 && y < 9 = 3 * (div x 3) + div y 3
	| otherwise = error "Posicion invalida, region no existe"

	
resolverSudoku :: Sudoku -> String
resolverSudoku sudoku@(l,o)
	| null l || null tactics = "\n" ++ toStringOcupadas o
	| otherwise = show tactica ++ "\n" ++ resolverSudoku (aplicarTactica sudoku tactica)
		where
			tactics = unicos l ++ exclusivos l ++ paresDescubiertos l ++ paresEscondidos l ++ ternasDescubiertas l ++ encerradoRegionFila l ++ encerradoRegionColumna l ++ encerradoFilaRegion l ++ encerradoColumnaRegion l 
			tactica = head tactics

 		
aplicarTactica :: Sudoku -> Tactica -> Sudoku
aplicarTactica (libres,ocupadas) (Unico pos num) = (updateLibres libres pos num, updateOcupadas ocupadas pos num)  
aplicarTactica (libres,ocupadas) (Exclusivo pos num _) = (updateLibres libres pos num, updateOcupadas ocupadas pos num)
aplicarTactica (libres,ocupadas) (Descubiertos _ nums celdas) = (updateLibresCeldas libres nums celdas, ocupadas)
aplicarTactica (libres,ocupadas) (Escondidos poss nums) = (updateLibresPE libres poss nums, ocupadas)
aplicarTactica (libres,ocupadas)  (Encerrado tipo (a) (b) num pos) = (updateLibresE libres pos num,ocupadas)

								
main :: IO ()
main = do
	entrada <- getContents
	let
		sudoku = cargarSudoku entrada (0,0) ([],newOcupadas (0,0))
		salida = resolverSudoku sudoku
	putStr salida

	

	
	