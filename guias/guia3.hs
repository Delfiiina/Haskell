-- Ejercicio 1
f :: Integer -> Integer
f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16 
-- b)
g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1
-- c)
h :: Integer -> Integer
h x = f (g x)
k :: Integer -> Integer
k x = g (f x)
-- Ejercicio 2
-- a)
absoluto :: Integer -> Integer
absoluto x
    | x == 0 = 0
    | x > 0 = x
    | x < 0 = -x
-- b)
maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto x y
    | absoluto x == absoluto y || absoluto x > absoluto y = absoluto x
    | absoluto x < absoluto y = absoluto y
-- c)
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 a b c 
    | a > b && a > c = a
    | b > a && b > c = b
    | c > a && c > b = c
-- d)
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y 
    | x == 0 || y == 0 = True
    | otherwise = False
algunoEs02 :: Float -> Float -> Bool
algunoEs02 0 _ = True
algunoEs02 _ 0 = True
algunoEs02 _ _ = False
-- e)
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y 
    | x == 0 && y == 0 = True
    | otherwise = False
ambosSon02 :: Float -> Float -> Bool
ambosSon02 0 0 = True
ambosSon02 _ _ = False
-- f)
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y 
    | x <= 3 && y <= 3 = True
    | x > 7 && y > 7 = True
    | x > 3 && x <= 7 && y > 3 && y <= 7 = True
    | otherwise = False
-- g)
sumaDistintos :: Float -> Float -> Float -> Float
sumaDistintos a b c 
    | a == b && b == c = 0
    | a /= b && b /= c && a /= c = a + b + c
    | a == b && c /= b && c /= a = a + c
    | a == c && b /= c && b /= a = c + b
    | c == b && a /= b && a /= c = a + b
-- h)
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y 
    | x `mod` y == 0 = True
    | otherwise = False
-- i)
digitoUnidades :: Integer -> Integer
digitoUnidades x = x `mod` 10 
-- j)
digitoDecenas