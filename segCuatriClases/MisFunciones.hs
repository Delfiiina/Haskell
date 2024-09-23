module MisFunciones where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

suma :: Int -> Int -> Int
suma x y = x + y

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
    | (mod x 2 /= 0) || (mod x 2 == 0) && (pertenece x xs) = pares xs
    | (mod x 2 == 0) && not (pertenece x xs) = x : pares xs

pertenece :: Integer -> [Integer] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs
 {--
pares :: [Integer] -> [Integer]
pares [] = []
pares [x]
    | x `mod` 2 == 0 = [x]
    | otherwise = []
pares (x:xs)
    | esPar x = sacarRepetidos(x : pares xs)
    | otherwise = sacarRepetidos(pares xs)

esPar :: Integer -> Bool
esPar x = x `mod` 2 == 0 

sacarRepetidos :: [Integer] -> [Integer]
sacarRepetidos [] = []
sacarRepetidos [x] = [x]
sacarRepetidos (x:xs) 
    | not (pertenece x xs) = [x] ++ sacarRepetidos xs
    | otherwise = x : (sacarRepetidos (sacar x xs) )

sacar :: (Eq t) => t -> [t] -> [t]
sacar _ [] = []
sacar n [x] 
    | n == x = []
    | otherwise = [x]
sacar n (x:xs)
    | n == x = sacar n xs
    | otherwise = x : (sacar n xs) 

pertenece :: Integer -> [Integer] -> Bool
pertenece _ [] = False
pertenece e (x:xs) = e == x || pertenece e xs
--}

--generarStock :: [String] -> [(String, Int)]
--generarStock [] = []
--generarStock (x:xs)  