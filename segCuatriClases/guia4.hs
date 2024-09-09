-- EJS CORREGIDOS = 2),7) y el 9) 
-- Ejercicio 2
parteEntera :: Float -> Integer
parteEntera n
    | 0 <= n && n < 1 = 0
    | otherwise = 1 + parteEntera (n-1)
--Ejercicio 7 
--iesimoDigito :: Integer -> Integer -> Integer
--iesimoDigito n i = ((n `div` (10 ^ ((cantDigitos n) - i))) `mod` 10) SIN RECURSIÓN
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n 1 = n `div` (10 ^ (cantDigitos(n) - 1))
iesimoDigito n i = iesimoDigito (n `mod` (10 ^ (cantDigitos(n)-1))) (i-1)
cantDigitos :: Integer -> Integer
cantDigitos 0 = 1
cantDigitos n 
    | 0 < n && n <= 9 = 1
    | otherwise = 1 + cantDigitos (n `div` 10)
--Ejercicio 9
esCapicua :: Integer -> Bool
esCapicua x
    | 0 <= x && x < 9 = True
    | otherwise = not ( (mod x 10 == (x `mod` (10 ^ ((cantDigitos x) - 1))))  && (esCapicua ((x `mod` 10)  `mod` (10 ^ ((cantDigitos x) - 1)))))
    -- Esta no funciona correctamente
esCapicuaBien :: Integer -> Bool
esCapicuaBien x
    | x < 10 = True
    | x < 100 = iesimoDigito x 1 == iesimoDigito x 2
    | otherwise = iesimoDigito x 1 == ultimoDigito && esCapicuaBien sacarPrimeroYUltimo
    where ultimoDigito = mod x 10
          sacarPrimeroYUltimo = div (mod x (10 ^ ((cantDigitos x)-1))) 10
--Ejercicio 16 
--a)
menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = loDivide   
--b) 
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
