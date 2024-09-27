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
{--
valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = []
valoresDeCamino t (p : ps) = obtener t p : valoresDeCamino t ps

obtener :: Tablero -> Posicion -> Int
obtener ((x : _) : _) (1, 1) = x
obtener ((_ : xs) : fs) (n, m)
  | n > 1 = obtener fs (n - 1, m) -- me desplazo hacia la derecha
  | otherwise = obtener (xs : fs) (1, m - 1) -- me desplazo hacia abajo

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
--}

-- Mio :P
valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = []
valoresDeCamino (y:ys) ((a,b):xs) = (recorroColumna b 1 (recorroFila a 1 (y:ys))): valoresDeCamino (y:ys) xs

recorroFila :: Int -> Int -> Tablero -> Fila
recorroFila _ _ [] = []
recorroFila a m (x:xs)
    | a == m = x
    | otherwise = recorroFila a (m+1) xs

recorroColumna :: Int -> Int -> Fila -> Int
recorroColumna a m (x:xs)
    | a == m = x
    | otherwise = recorroColumna a (m+1) xs


-- Ejercicio 8 
fibonacci :: Int -> Int
fibonacci x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fibonacci (x-1) + fibonacci (x-2)

esCaminoFibo :: [Int] -> Int -> Bool
esCaminoFibo [] _ = True 
esCaminoFibo (x:xs) i = not (i /= x) && ((x:xs) == secFiboDesde x (ultimoElemento xs))
    where ultimoElemento (x:xs) = head (reverso (x:xs))

reverso :: [Int] -> [Int]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

perteneceAFibo :: Int -> Int -> Bool
perteneceAFibo n x 
    | fibonacci x == n = True
    | fibonacci x < n = perteneceAFibo n (x+1)
    | otherwise = False

secFiboDesde :: Int -> Int -> [Int]
secFiboDesde x n
    | fibonacci (x) <= n = (fibonacci x) : secFiboDesde (x+1) n
    | otherwise = []

-- Ejercicio 9
divisoresPropios :: Int -> [Int]
--divisoresPropios x = pasarANegativoYdarVuelta (divisoresAux x 1) ++ divisoresAux x 1
divisoresPropios x =  divisoresAux x 1


divisoresAux :: Int -> Int -> [Int]
divisoresAux x a
    | a >= x = []
    | mod x a == 0 = [a] ++ divisoresAux x (a + 1)
    | otherwise = divisoresAux x (a + 1)


{--
pasarANegativoYdarVuelta :: [Int] -> [Int]
pasarANegativoYdarVuelta [] = []
pasarANegativoYdarVuelta (x:xs) = pasarANegativoYdarVuelta xs ++ [-x]
--}


-- Ejercicio 10
sumaDeDivisores :: [Int] -> Int
sumaDeDivisores [] = 0
sumaDeDivisores (x:xs) = x + sumaDeDivisores xs

sumaDeDivisoresPropios :: Int -> Int
sumaDeDivisoresPropios 1 = 0
sumaDeDivisoresPropios a = sumaDeDivisores (divisoresPropios a)

sonAmigos :: Int -> Int -> Bool
sonAmigos a b = (sumaDeDivisoresPropios a) == b && (sumaDeDivisoresPropios b) == a

-- Ejercicio 11 

losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = losPrimerosNPerfectosAux 1 n

losPrimerosNPerfectosAux :: Int -> Int -> [Int]
losPrimerosNPerfectosAux n 0 = []
losPrimerosNPerfectosAux n m 
    | esNPerfecto n = n : losPrimerosNPerfectosAux (n+1) (m-1)
    | otherwise = losPrimerosNPerfectosAux (n+1) m

esNPerfecto :: Int -> Bool
esNPerfecto x = x == sumaDeDivisoresVersion2 x 1 
   
sumaDeDivisoresVersion2 :: Int -> Int -> Int
sumaDeDivisoresVersion2 1 _ = 1
sumaDeDivisoresVersion2 n m 
    | n == m = 0
    | mod n m == 0 = m + sumaDeDivisoresVersion2 n (m+1)
    | otherwise = sumaDeDivisoresVersion2 n (m+1)

-- Ejercicio 12

listaDeAmigos :: [Int] ->[(Int,Int)]
listaDeAmigos [] = []
listaDeAmigos (x:xs) 
    | esAmigoDe x (x:xs) = (x, hallarSuAmigo x (x:xs)) : listaDeAmigos xs
    | otherwise = listaDeAmigos xs

hallarSuAmigo :: Int -> [Int] -> Int
hallarSuAmigo a (x:xs)
    | sonAmigos a x = x
    | otherwise = hallarSuAmigo a xs


esAmigoDe :: Int -> [Int] -> Bool
esAmigoDe _ [] = False
esAmigoDe a (x:xs)
    | sonAmigos a x = True
    | otherwise = esAmigoDe a xs