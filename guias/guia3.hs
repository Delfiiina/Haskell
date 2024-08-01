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
-- forma usando funciones no permitidas
maximoabsoluto2 x y = max (absoluto x) (absoluto y)
-- c)
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 a b c 
    | a > b && a > c = a
    | b > a && b > c = b
    | c > a && c > b = c
-- forma usando funciones no permitidas
maximo32 a b c = max (max b c) a
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
digitoDecenas :: Integer -> Integer
digitoDecenas x = (x `div` 10) `mod` 10
-- Ejercicio 4) a)
prodInt :: (Integer,Integer) -> (Integer,Integer) -> Integer
prodInt (a1,a2) (b1,b2) = a1*b1 + a2*b2
-- b)
todoMenor :: (Integer,Integer) -> (Integer,Integer) -> Bool
todoMenor (a1,a2) (b1,b2) 
    | a1 < b1 && a2 < b2 = True
    | otherwise = False
-- d)
sumaTerna :: (Integer,Integer,Integer) -> Integer
sumaTerna (a,b,c) = a+b+c
-- e)
sumarSoloMultiplos :: (Integer,Integer,Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) x 
    | a `mod` x /= 0 && b `mod` x /= 0 && c `mod` x /= 0 = 0
    | a `mod` x == 0 && b `mod` x == 0 && c `mod` x == 0 = a + b + c
    | a `mod` x /= 0 && b `mod` x == 0 && c `mod` x == 0 = b + c
    | a `mod` x == 0 && b `mod` x /= 0 && c `mod` x == 0 = a + c
    | a `mod` x == 0 && b `mod` x == 0 && c `mod` x /= 0 = a + b 
    | a `mod` x /= 0 && b `mod` x /= 0 && c `mod` x == 0 =  c
    | a `mod` x == 0 && b `mod` x /= 0 && c `mod` x /= 0 =  a
    | a `mod` x == 0 && b `mod` x == 0 && c `mod` x /= 0 =  b
-- f)
posPrimerPar :: (Integer,Integer,Integer) -> Integer
posPrimerPar (a,b,c)
    | a `mod` 2 == 0 = 1
    | b `mod` 2 == 0 = 2
    | c `mod` 2 == 0 = 3
    | otherwise = 4
-- g)
crearPar :: x -> y -> (x,y)
crearPar x y = (x,y)
-- h)
invertir :: (a,b) -> (b,a)
invertir (a,b) = (b,a)
-- Ejercicio 5
f2 :: Integer -> Integer
f2 n 
    | n <= 7 = n*n
    | otherwise = 2*n - 1
g2 :: Integer -> Integer
g2 n
    | n `mod` 2 == 0 = n `div`2
    | otherwise = 3*n + 1
todosMenores :: (Integer,Integer,Integer) -> Bool
todosMenores (a,b,c)
    |f2 a > g2 a && f2 b > g2 b && f2 c > g2 c = True
    | otherwise = False
-- Ejercicio 6 
bisiesto :: Integer -> Bool
bisiesto x
    | x `mod` 4 /= 0 || (x `mod` 100 == 0 && x `mod` 400 /= 0) = False
    | otherwise = True
-- Ejercicio 7 
distanciaManhattan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhattan (p0,p1,p2) (q0,q1,q2) = absolutoabs (p0-q0) + absolutoabs (p1-q1) + absolutoabs (p2-q2)
-- Creo la misma función de absoluto de antes pero ahora con Float así la puedo utilizar en esta función
absolutoabs :: Float -> Float
absolutoabs a
    | a == 0 = 0
    | a > 0 = a 
    | a < 0 = -a
-- Ejercicio 8
sumaUltimosDigitos :: Integer -> Integer
sumaUltimosDigitos x = (absoluto x) `mod` 10 + ((absoluto x `div` 10) `mod` 10)
comparar :: Integer -> Integer -> Integer
comparar a b 
    | sumaUltimosDigitos a < sumaUltimosDigitos b = 1
    | sumaUltimosDigitos a > sumaUltimosDigitos b = -1
    | sumaUltimosDigitos a == sumaUltimosDigitos b = 0
    