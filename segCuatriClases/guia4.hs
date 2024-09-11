-- Ejercicio 16
-- a)
menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = loDivide n 2 
loDivide :: Integer -> Integer -> Integer
loDivide n x 
    | mod n x == 0 = x
    | otherwise = loDivide n (x+1)
-- b)
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = verSiesPrimo n (n-1)
verSiesPrimo :: Integer -> Integer -> Bool
verSiesPrimo _ 1 = True
verSiesPrimo n m = (not (esDivisible n m)) && (verSiesPrimo n (m-1)) 
esDivisible :: Integer -> Integer -> Bool
esDivisible x y 
    | (x - y) == 0 = True
    | (x - y) < 0 = False
    | (x - y) /= 0 = esDivisible (x-y) y 
esPrimo2 :: Integer -> Bool
esPrimo2 1 = False
esPrimo2 n = menorDivisor n == n
-- c) 
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b 
    | a == b = False
    | a == 1 || b == 1 = True
    | otherwise = verSiSonCoprimos a b 2
verSiSonCoprimos :: Integer -> Integer -> Integer -> Bool
verSiSonCoprimos a b n
    | mod a n == 0 && mod b n == 0 = False
    | otherwise = verSiSonCoprimos a b (n+1)
-- Ejercicio 14
{--
sumaPotencias :: Integer ->Integer ->Integer ->Integer
sumaPotencias q 1 1 = q ^ 2
sumaPotencias q n m = (sumaPotenciasFijoM q n m) + (sumaPotenciasFijoN q n m)

sumaPotenciasFijoM :: Integer -> Integer -> Integer -> Integer
sumaPotenciasFijoM q 1 m = q ^ (1+m)
sumaPotenciasFijoM q n m = (q ^ (n+m)) + (sumaPotenciasFijoM q (n-1) m) 

sumaPotenciasFijoN :: Integer -> Integer -> Integer -> Integer
sumaPotenciasFijoN q n 1 = q ^ (n +1) 
sumaPotenciasFijoN q n m = (q ^ (n+m)) + (sumaPotenciasFijoN q n (m-1))
--}
-- No era necesario tener 2 funciones auxiliares.
sumaPotencias :: Integer ->Integer ->Integer ->Integer
sumaPotencias q 1 1 = q ^ 2
sumaPotencias q 1 m = sumaPotenciasFijoM q 1 m 
sumaPotencias q n m = (sumaPotenciasFijoM q n m) + sumaPotencias q (n-1) m

sumaPotenciasFijoM :: Integer -> Integer -> Integer -> Integer
sumaPotenciasFijoM q n 0 = 0
sumaPotenciasFijoM q n 1 = q ^ (1+n)
sumaPotenciasFijoM q n m = (q ^ (n+m)) + (sumaPotenciasFijoM q n (m-1)) 

-- Ejercicio 19
esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos 