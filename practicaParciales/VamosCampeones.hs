-- Ejercicio 2

equiposValidos :: [(String,String)] -> Bool
equiposValidos [] = True
equiposValidos ((a,b):xs) = noTieneNombreDeClub (a,b) && noSeRepiten (a,b) xs && equiposValidos xs

noTieneNombreDeClub :: (String,String) -> Bool
noTieneNombreDeClub (a,b) = a/= b 

noSeRepiten :: (String,String) -> [(String,String)] -> Bool
noSeRepiten _ [] = True
noSeRepiten (a,b) ((c,d):xs) = noSeRepite a ((c,d):xs) && noSeRepite b ((c,d):xs) && noSeRepiten (a,b) xs

noSeRepite :: String -> [(String,String)] -> Bool
noSeRepite _ [] = True
noSeRepite a ((c,d):xs) = (a /= c) && (a /= d) && noSeRepite a xs

-- Ejercicio 1

atajaronSuplentes :: [(String,String)] -> [Int] -> Int -> Int
atajaronSuplentes [] [] _ = 0
atajaronSuplentes ((a,b):xs) (y:ys) g = g - (sumarTodosGoles (y:ys))

sumarTodosGoles :: [Int] -> Int
sumarTodosGoles [] = 0
sumarTodosGoles (x:xs) = x + sumarTodosGoles xs

-- Ejercicio 3

division :: Int -> Int -> Float
division a b = fromIntegral a / fromIntegral b

porcentajeDeGoles :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeGoles _ [] [] = 0
porcentajeDeGoles a ((c,d):xs) (y:ys) = division (golesDe a ((c,d):xs) (y:ys) * 100) (sumarTodosGoles (y:ys))

golesDe :: String -> [(String,String)] -> [Int] -> Int
golesDe _ [] [] = 0
golesDe a ((c,d):xs) (y:ys)
    | a == c = y 
    | otherwise = golesDe a xs ys 

-- Ejercicio 4 
-- voy a suponer que es el arquero que más goles atajó, es decir el mejor(?) por ende, el que menos goles recibió

vallaMenosVencida :: [(String,String)] -> [Int] -> String
vallaMenosVencida [x] [y] = fst x 
vallaMenosVencida ((a,b):(c,d):xs) (y:ys) 
    | porcentajeDeGoles a ((a,b):(c,d):xs) (y:ys) <= porcentajeDeGoles c ((a,b):(c,d):xs) (y:ys) = vallaMenosVencida ((a,b):xs) (sacarGolesDeEquipo (c,d) ((a,b):(c,d):xs) (y:ys))
    | otherwise = vallaMenosVencida ((c,d):xs) (sacarGolesDeEquipo (a,b) ((a,b):(c,d):xs) (y:ys))

sacarGolesDeEquipo :: (String,String) -> [(String,String)] -> [Int] -> [Int]
sacarGolesDeEquipo _ [] [] = []
sacarGolesDeEquipo (a,b) ((c,d):xs) (y:ys)
    | (a,b) == (c,d) = ys
    | otherwise = y : sacarGolesDeEquipo (a,b) xs ys 
