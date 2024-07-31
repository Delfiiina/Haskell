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
indice :: Integer -> [Float] -> Float
indice 0 (x:xs) = x
indice n (x:xs) = indice (n-1) xs
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
take2 n (x:xs) = x : take2 (n-1) xs
drop2 :: Integer -> [Float] -> [Float]