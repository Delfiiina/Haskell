-- Primeras funciones (creo que no les puedo dar el mismo nombre asi que se los cambio) 
-- Están hechas solo para números de cualquier tipo, pero se puede cambiar y con las funciones válidas
succ2 :: Float -> Float
succ2 x = x + 1
min2 :: Float -> Float -> Float
min2 x y 
    | x > y = y
    | x <= y = x
max2 :: Float -> Float -> Float
max2 x y
    | x > y = x
    | x <= y = y
-- Listas
indice :: Integer -> [Float] -> Float
indice 0 (x:xs) = x
indice n (x:xs) = indice (n-1) xs
-- Funciones básicas para listas
last2 :: [Float]  -> Float
last2 [x] = x
last2 (x:xs) = last2 xs 
init2 :: [Float] -> [Float]
init2 [x] = []
init2 (x:xs) = x: init2 xs
length2 :: [Float] -> Integer
length2 [] = 0
length2 (x:xs) = 1 + length2 xs 
null2 :: [Float] -> Bool
null2 [] = True
null2 (_:_) = False
reverse2 :: [Float] -> [Float]
reverse2 [] = []
reverse2 [x] = [x]
reverse2 (x:xs) = reverse2 xs ++ [x]
take2 :: Integer -> [Float] -> [Float]
take2 0 (_:_) = []
take2 n [x] = [x]
take2 n (x:xs) = x : take2 (n-1) xs
drop2 :: Integer -> [Float] -> [Float]
drop2 0 (x:xs) = (x:xs)
drop2 n [] = []
drop2 n [x] = []
drop2 n (x:xs) = drop2 (n-1) (tail (x:xs))
maximum2 :: [Float] -> Float
maximum2 [x] = x 
maximum2 (x:xs)
    | x >= head xs = maximum2 (x:tail xs) 
    | x < head xs = maximum2 xs 
minimum2 :: [Float] -> Float
minimum2 [x] = x
minimum2 (x:xs)
    | x <= head xs = minimum2 (x:tail xs)
    | x > head xs = minimum2 xs
sum2 :: [Float] -> Float
sum2 [] = 0
sum2 [x] = x
sum2 (x:xs) = x + sum2 xs
product2 :: [Float] -> Float
product2 [] = 0
product2 [x] = x
product2 (x:xs) = x * product2 xs 
elem2 :: Float -> [Float] -> Bool
elem2 _ [] = False
elem2 n [x] 
    | n == x = True
    | otherwise = False
elem2 n (x:xs) 
    | n == x = True
    | otherwise = elem2 n xs