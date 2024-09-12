-- Recursión sobre números enteros
-- Ejercicio 1
fibonacci :: Integer -> Integer 
fibonacci x
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = fibonacci (x-1) + fibonacci (x-2) 
{-- Ejercicio 2
parteEntera :: Float -> Integer 
esDivisible 0 = 0
esDivisible x
    | x > 0 =  
        --}
-- Ejercicio 3
esDivisible :: Integer -> Integer -> Bool
esDivisible x y 
    | x - y == 0 = True
    | x - y < 0 = False
    | x - y /= 0 = esDivisible (x-y) y 
-- Ejercicio 4
sumaImpares :: Integer -> Integer 
sumaImpares 0 = 0
sumaImpares 1 = 1
sumaImpares x = sumaImpares (x-1) + (2*x -1) 
-- Ejercicio 5 
medioFact:: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact (x-2)
-- Ejercicio 6
sumaDigitos :: Integer -> Integer
sumaDigitos x 
    | 0 <= x && x <= 9 = x
    | otherwise = (x `mod` 10) + sumaDigitos (x `div` 10)
-- Ejercicio 7
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x
    | 0 < x && x < 10 = True
    | otherwise =  (x `mod` 10) == ((x `div` 10) `mod` 10) && todosDigitosIguales (x `div` 10)
-- Ejercicio 8
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = ((n `div` (10 ^ ((cantDigitos n) - i))) `mod` 10)
cantDigitos :: Integer -> Integer
cantDigitos 0 = 1
cantDigitos n 
    | 0 < n && n <= 9 = 1
    | otherwise = 1 + cantDigitos (n `div` 10)
-- Ejercicio 9
esCapicua :: Integer -> Bool
esCapicua x
    | 0 <= x && x < 9 = True
    | otherwise = not ( (mod x 10 == (x `mod` (10 ^ ((cantDigitos x) - 1))))  && (esCapicua ((x `mod` 10)  `mod` (10 ^ ((cantDigitos x) - 1)))))
-- Ejercicio 10
-- a) 
f1 :: Int -> Int
f1 x = 2 ^ (x+1) - 1
-- b)
{--f2 :: Int -> Int -> Int
f2 n 1 = n
f2 n q = (q^(n+1)-q)/(q-1)--}
-- Ejercicio 11
--a)
eAprox :: Integer -> Float
eAprox 1 = fromIntegral 2
eAprox n = eAprox(n-1) + (1/fromIntegral(factorial n))
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
--b)
e :: Float
e = eAprox 10
-- Ejercicio 12
raizDe2Aprox :: Integer -> Float
raizDe2Aprox 1 = fromIntegral 1
raizDe2Aprox n = (raizDe2AproxAux n) - 1
raizDe2AproxAux 1 = fromIntegral 2
raizDe2AproxAux n = (2 + (1/(raizDe2AproxAux (n-1))))
-- Ejercicio 13
--sumatoriaInterna :: Integer -> Integer -> Integer
-- Ejercicio 14
sumaPotencias :: Integer ->Integer ->Integer ->Integer
sumaPotencias q 1 1 = q ^ 2
sumaPotencias q n m = (sumaPotenciasFijoM q n m) + (sumaPotenciasFijoN q n m)
sumaPotenciasFijoM :: Integer -> Integer -> Integer -> Integer
sumaPotenciasFijoM q 1 m = q ^ (1+m)
sumaPotenciasFijoM q n m = (q ^ (n+m)) + (sumaPotenciasFijoM q (n-1) m) 
sumaPotenciasFijoN :: Integer -> Integer -> Integer -> Integer
sumaPotenciasFijoN q n 1 = q ^ (n +1) 
sumaPotenciasFijoN q n m = (q ^ (n+m)) + (sumaPotenciasFijoN q n (m-1))
-- Ejercicio 15
-- Ejercicio 16 
