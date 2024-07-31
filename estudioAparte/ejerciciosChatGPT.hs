{--
Ejercicio 1: Manipulación de listas
Crea una función removeDuplicates que tome una lista y devuelva una nueva lista sin elementos duplicados. Solo puedes usar las funciones básicas de listas y concatenación
removeDuplicates :: Eq a => [a] -> [a]
Ejercicio 2: Análisis de listas
Crea una función maxInList que encuentre el valor máximo en una lista de números utilizando solo las funciones básicas de listas.
maxInList :: Ord a => [a] -> a
Ejercicio 3: Números primos
Escribe una función isPrime que determine si un número es primo y una función primesUpTo que genere una lista de todos los números primos hasta un número dado utilizando rangos y funciones básicas de listas.
isPrime :: Int -> Bool
primesUpTo :: Int -> [Int]
Ejercicio 4: Operaciones sobre listas infinitas
Escribe una función firstNPrimes que tome un entero n y devuelva una lista con los primeros n números primos utilizando listas infinitas.
firstNPrimes :: Int -> [Int]
Ejercicio 5: Combinación y permutación de listas
Escribe una función combinations que genere todas las combinaciones posibles de un tamaño dado de una lista y una función permutations que genere todas las permutaciones posibles de una lista.
combinations :: Int -> [a] -> [[a]]
permutations :: [a] -> [[a]]
Ejercicio 6: Implementación de ciclos
Escribe una función cycleN que tome un número n y una lista, y devuelva una nueva lista con la lista original repetida n veces.
cycleN :: Int -> [a] -> [a]
--}

-- Ejercicio 1
removeDuplicates :: [Integer] -> [Integer] 
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs
-- Ejercicio 2
maxInList :: [Integer] -> Integer
maxInList [x] = x
maxInList (x:xs)
    | x > head xs = maxInList (x:tail xs)
    | otherwise = maxInList xs 
-- Ejercicio 3
isPrime :: Integer -> Bool
