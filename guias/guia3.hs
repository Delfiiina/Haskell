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
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 a b c 
    | a > b && a > c = a
    | b > a && b > c = b
    | c > a && c > b = c