-- Ejercicio 1

generarStock :: [String] -> [(String, Int)]
generarStock [] = []
generarStock (x:xs) = (x, (contarProductos x (x:xs))) : generarStock (eliminarProducto x (x:xs))

contarProductos :: String -> [String] -> Int
contarProductos _ [] = 0
contarProductos a (x:xs) 
    | a == x = 1 + contarProductos a xs
    | otherwise = contarProductos a xs 

eliminarProducto :: String -> [String] -> [String]
eliminarProducto _ [] = []
eliminarProducto a (x:xs)
    | a == x = eliminarProducto a xs
    | otherwise = x : eliminarProducto a xs 

-- Ejercicio 2

stockDeProducto :: [(String,Int)] -> String -> Int
stockDeProducto [] _ = 0
stockDeProducto ((c,d):xs) a 
    | a == c = d 
    | otherwise = stockDeProducto xs a 

-- Ejercicio 3

buscaPrecio :: (String,Int) -> [(String,Float)] -> Float
buscaPrecio _ [] = fromIntegral 0
buscaPrecio (a,b) ((c,d):xs)
    | a == c = d 
    | otherwise = buscaPrecio (a,b) xs

dineroEnStock :: [(String,Int)] -> [(String,Float)] -> Float
dineroEnStock [] _ = fromIntegral 0
dineroEnStock _ [] = fromIntegral 0
dineroEnStock ((p1,st1):xs) ((pr1,pre1):ys) = (buscaPrecio (p1,st1) ((pr1,pre1):ys) ) * (fromIntegral st1) + dineroEnStock xs ((pr1,pre1):ys)

-- Ejercicio 4


aplicarOferta :: [(String,Int)] -> [(String,Float)] -> [(String,Float)]
aplicarOferta [] _ = []
aplicarOferta _ [] = []
aplicarOferta ((a,b):xs) precios 
    | (buscaPrecio (a,b) precios) > fromIntegral 10 = (a,((buscaPrecio (a,b) precios) * 0.80)) : aplicarOferta xs precios
    | otherwise = (a,(buscaPrecio (a,b) precios)) : aplicarOferta xs precios

-- Ejercicio 5


type Fila = [Int]
type Tablero = [Fila]
type Posicion = (Int,Int)
type Camino = [Posicion]


mayorDeLaFila :: Fila -> Int
mayorDeLaFila [x] = x 
mayorDeLaFila (x:y:xs)
    | x >= y = mayorDeLaFila (x:xs)
    | otherwise = mayorDeLaFila (y:xs)

maximo :: Tablero -> Int
maximo [x] = mayorDeLaFila x
maximo (x:y:xs)
    | mayorDeLaFila x >= mayorDeLaFila y = maximo (x:xs)
    | otherwise = maximo (y:xs)

-- Ejercicio 6
masRepetido :: Tablero -> Int
masRepetido [x] = masAparicionesEnFila x
masRepetido (x:xs) = masRepetidoAux (plancharTablero (x:xs))

cantAparicionesEnFila :: Int -> Fila -> Int
cantAparicionesEnFila a [x] 
    | a == x = 1
    | otherwise = 0
cantAparicionesEnFila a (x:xs)
    | a == x = 1 + cantAparicionesEnFila a xs
    | otherwise = cantAparicionesEnFila a xs

plancharTablero :: Tablero -> [Int]
plancharTablero [] = []
plancharTablero (x:xs) = x ++ plancharTablero xs



masAparicionesEnFila ::  Fila -> Int
masAparicionesEnFila [x] = x
masAparicionesEnFila (x:y:xs)
    | cantAparicionesEnFila x (x:y:xs) >= cantAparicionesEnFila y (x:y:xs) = masAparicionesEnFila (x:xs)
    | otherwise = masAparicionesEnFila (y:xs)

masRepetidoAux  :: [Int] -> Int
masRepetidoAux [x] = x
masRepetidoAux (x:y:xs)
    | cantAparicionesEnFila x (x:y:xs) >= cantAparicionesEnFila y (x:y:xs) = masRepetidoAux (x:xs)
    | otherwise = masRepetidoAux (y:xs)