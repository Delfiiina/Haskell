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
