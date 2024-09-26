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
masRepetido (x:xs) = buscarTuplaMayor  (tuplarPorCantidades (plancharTablero (x:xs)))

buscarTuplaMayor :: [(Int,Int)] -> Int  -- dada la lista con tuplas (elemento,cant de apariciones) devuelve el elemento con cant de apariciones mayor
buscarTuplaMayor [(a,b)] = a
buscarTuplaMayor ((a,b):(c,d):xs)
    | b >= d = buscarTuplaMayor ((a,b):xs)
    | otherwise = buscarTuplaMayor ((c,d):xs)

cantAparicionesEnFila :: Int -> Fila -> Int
cantAparicionesEnFila a [x] 
    | a == x = 1
    | otherwise = 0
cantAparicionesEnFila a (x:xs)
    | a == x = 1 + cantAparicionesEnFila a xs
    | otherwise = cantAparicionesEnFila a xs

tuplarPorCantidades :: Fila -> [(Int,Int)]
tuplarPorCantidades [] = []
tuplarPorCantidades (x:xs) = (x, cantAparicionesEnFila x (x:xs)) : (tuplarPorCantidades (eliminarElementoDeFila x (x:xs)))

eliminarElementoDeFila :: Int -> Fila -> Fila
eliminarElementoDeFila _ [] = []
eliminarElementoDeFila a (x:xs)
    | a == x = eliminarElementoDeFila a xs
    | otherwise = x : eliminarElementoDeFila a xs 

plancharTablero :: Tablero -> [Int]
plancharTablero [] = []
plancharTablero (x:xs) = x ++ plancharTablero xs

masAparicionesEnFila ::  Fila -> Int
masAparicionesEnFila [x] = x
masAparicionesEnFila (x:y:xs)
    | cantAparicionesEnFila x (x:y:xs) >= cantAparicionesEnFila y (x:y:xs) = masAparicionesEnFila (x:xs)
    | otherwise = masAparicionesEnFila (y:xs)


-- Ejercicio 7

valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = []
valoresDeCamino _ (x:xs) = destuplar (ordenarPorSegundaComponente(ordenarPorPrimeraComponente (x:xs)))

ordenarPorPrimeraComponente :: Camino -> Camino
ordenarPorPrimeraComponente [] = []
ordenarPorPrimeraComponente [x] = [x]
ordenarPorPrimeraComponente ((a,b):(c,d):xs)
    | a <= c = (a,b) : ordenarPorPrimeraComponente ((c,d):xs)
    | otherwise = (c,d) : ordenarPorPrimeraComponente ((a,b):xs)

ordenarPorSegundaComponente :: Camino -> Camino
ordenarPorSegundaComponente [] = []
ordenarPorSegundaComponente [x] = [x]
ordenarPorSegundaComponente ((a,b):(c,d):xs)
    | b <= d = (a,b) : ordenarPorSegundaComponente ((c,d):xs)
    | otherwise = (c,d) : ordenarPorSegundaComponente ((a,b):xs)

destuplar :: Camino -> [Int]
destuplar [] = []
destuplar ((a,b):xs) = a : b : destuplar xs

-- Ejercicio 8 
fibonacci :: Integer -> Integer 
fibonacci x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fibonacci (x-1) + fibonacci (x-2)
