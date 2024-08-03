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
--todosDigitosIguales :: Integer -> Bool
--todosDigitosIguales x
--    | 0 <= x && x <= 9 = True
--    | otherwise = (x `mod` 10) == todosDigitosIguales (x `div` 10)
