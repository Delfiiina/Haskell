-- Fecha: 02/09/2024
doubleMe :: Int -> Int
doubleMe x = x + x
-- Moverse en la terminal
-- pwd te dice dónde estás parado en el directorio/ qué carpeta
-- cd moverse de carpetas
-- cd .. volver para "atrás" en las carpetas-- ls es para 
-- Abris ghci y ahí cargas tu archivo :l *nombre*
-- Si agregás cosas primero cargas archivo ctrl + s y desp :r 
tripleMe :: Int -> Int
tripleMe x = x * 3
-- Para salir de ghci :q 
-- Le pasas una expresión a :t y te dice el tipo
-- GUÍA 3
-- Ej 1 
f :: Integer -> Integer
f x 
    | x == 1 = 8
    | x == 4 = 131
    | otherwise = 16
-- con PM
f2 :: Integer -> Integer
f2 1 = 8
f2 4 = 131
f2 16 = 16 
-- b)
g :: Integer -> Integer
g x 
    | x == 8 = 16
    | x == 16 = 4
    | x == 131 = 1
-- c) 
h :: Integer -> Integer
h x = f (g x) -- a f le tenes que dar 1 solo parámetro 

k :: Integer -> Integer
k x = g (f x)
-- Ej 2) c)
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z
    | x >= y && x >= z = x -- No es necesario meter a x en la comparación siguiente ya que si pasó a la sig. guarda significa q x no es el máx
    | y >= x && y >= z = y
    | z >= x && z >= y = z
maximoOpc2 :: Integer -> Integer -> Integer -> Integer
maximoOpc2 x y z
    | x >= y && x >= z = x
    | y >= z = y
    | otherwise = z
-- g)
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z
    | x /= y && x /= z && y /= z = x + y + z
    | x == y && x == z = x
    | x /= y && x == z = x + y 
    | x == y && x /= z = x + z
    | x /= y && y == z = x + y
sumaDistintos2 :: Integer -> Integer -> Integer -> Integer
sumaDistintos2 x y z 
    | x /= y && x /= z && y /= z = x + y + z
    | x == y && x == z = 0
    | x /= y && x == z = y 
    | x == y && x /= z = z
    | y /= x && y == z = x
-- el orden de las guardas !!!
sumaDistintos3 :: Integer -> Integer -> Integer -> Integer
sumaDistintos3 x y z
    | x /= y && x /= z && y /= z = x + y + z
    | x == y && x == z = 0
    | x == y = z
    | x == z = y
    | y == z = x
-- i)
digitoUnidades :: Integer -> Integer
digitoUnidades x = x `mod` 10 -- no me da el ult. dig. de los negativos ya q el mod es siempre positivo
digitoUnidades2 x 
    | x > 0 = x `mod` 10
    | otherwise = (-x) `mod` 10
-- Ej 4) b)
todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor (x,y) (a,b) 
    | x < a && y < b = True
    | otherwise = False -- Está de más? No ya que hay q "completar" lo de las guardas, sacarlo es directamente escribirlo como todoMenor2
-- Se puede hacer sin guardas, devolviendo directamente el valor de verdad
todoMenor2 :: (Float,Float) -> (Float,Float) -> Bool
todoMenor2 (x,y) (a,b) = x < a && y < b
-- También se puede hacer usando fst y snd (más complicado de leer y solo se puede usar con duplas)