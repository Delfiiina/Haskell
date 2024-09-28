-- Ejercicio 2

formulasValidas :: [(String,String)] -> Bool
formulasValidas [] = True
formulasValidas (x:xs) = noHayRepetidosTupla x && not (perteneceN x xs) && formulasValidas xs

noHayRepetidosTupla :: (String,String) -> Bool
noHayRepetidosTupla (a,b) = a /= b

perteneceN :: (String, String) -> [(String,String)] -> Bool
perteneceN _ [] = False
perteneceN (a,b) ((c,d):xs) = pertenePersona a ((c,d):xs) || pertenePersona b ((c,d):xs) || perteneceN (a,b) xs

pertenePersona :: String -> [(String,String)] -> Bool
pertenePersona _ [] = False
pertenePersona a ((c,d):xs) = (a == c || a == d) || pertenePersona a xs 

-- Ejercicio 1 

votosEnBlanco :: [(String,String)] -> [Int] -> Int -> Int
votosEnBlanco _ (x:xs) a = a - (sumarTodos (x:xs))

sumarTodos :: [Int] -> Int
sumarTodos [] = 0
sumarTodos (x:xs) = x + sumarTodos xs

-- Ejercicio 3
porcentajeDeVotos :: String -> [(String,String)] -> [Int] -> Float
porcentajeDeVotos _ [] [] = 0
porcentajeDeVotos a ((c,d):xs) (y:ys) = (fromIntegral ((votosDe a ((c,d):xs) (y:ys)) * 100)) / (fromIntegral (sumarTodos (y:ys)))

votosDe :: String -> [(String,String)] -> [Int] -> Int
votosDe _ [] [] = 0
votosDe a ((c,d):xs) (y:ys) 
    | a == c = y 
    | otherwise = votosDe a xs ys 

-- Ejercicio 4

proximoPresidente :: [(String,String)] -> [Int] -> String
proximoPresidente [x] _ = fst x
proximoPresidente ((a,b):(c,d):xs) (y:ys)
    | (porcentajeDeVotos a ((a,b):(c,d):xs) (y:ys) ) >= (porcentajeDeVotos c ((a,b):(c,d):xs) (y:ys) ) = proximoPresidente ((a,b):xs) (sacarVotosDe (c,d) ((a,b):(c,d):xs) (y:ys))
    | otherwise = proximoPresidente ((c,d):xs)  (sacarVotosDe (a,b)  ((a,b):(c,d):xs) (y:ys))

sacarVotosDe ::(String,String) -> [(String,String)] -> [Int] -> [Int]
sacarVotosDe _ [] [] = []
sacarVotosDe (a,b) ((c,d):xs) (y:ys)
    | (a,b) == (c,d) = ys
    | otherwise = y : sacarVotosDe (a,b) xs ys 