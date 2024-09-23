-- Ejercicio 1.1

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Ejercicio 1.2

ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs 

-- Ejercicio 1.3

principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs 

-- Ejercicio 1.4

reverso :: [t] -> [t]
reverso [] = []
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

-- Ejercicio 2.1

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) 
    | n == x = True
    |otherwise = pertenece n xs

-- Ejercicio 2.2

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs

-- Ejercicio 2.3

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs

-- Ejercicio 2.4

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = not (todosDistintos (x:xs))

-- Ejercicio 2.5

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
{--quitar n [x] 
    | n == x = []
    | otherwise = [x]--}
quitar n (x:xs)
    | n == x = xs -- Si yo pongo, en vez de xs, quitar n xs me va a sacar todos los n q encuentre. De esta forma me saca solo el primero q encuentra 
    | otherwise = x : (quitar n xs) 

-- Ejercicio 2.6

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n [x] 
    | n == x = []
    | otherwise = [x]
quitarTodos n (x:xs)
    | n == x = quitarTodos n xs
    | otherwise = x : (quitarTodos n xs) 

-- Ejercicio 2.7

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) 
    | not (pertenece x xs) = [x] ++  eliminarRepetidos xs
    | otherwise = x : (eliminarRepetidos (quitarTodos x xs))   

-- Ejercicio 2.8

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] _ = False
mismosElementos _ [] = False
mismosElementos [x] [y] = x == y
mismosElementos (x:xs) (y:ys) = pertenecenTodos (eliminarRepetidos(x:xs)) (eliminarRepetidos(y:ys)) && pertenecenTodos (y:ys) (x:xs)

pertenecenTodos :: (Eq t) => [t] -> [t] -> Bool
pertenecenTodos [] [] = True
pertenecenTodos [] _ = True
pertenecenTodos _ [] = True
pertenecenTodos [x] [y] = x == y
pertenecenTodos (x:xs) (y:ys) = pertenece x (y:ys) && pertenecenTodos xs (y:ys)

-- Ejercicio 2.9

capicua :: (Eq t) => [t] -> Bool 
capicua [] = True
capicua [x] = True
capicua (x:xs) = head (x:xs) == head (reverso (x:xs)) && capicua (principio xs)

-- Ejercicio 3.1

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

-- Ejercicio 3.2
productoria :: [Integer] -> Integer
productoria [] = 1
productoria [x] = x
productoria (x:xs) = x * productoria xs

-- Ejercicio 3.3

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) 
    | x >= y = maximo (x:xs)
    | otherwise = maximo (y:xs)

-- Ejercicio 3.4

sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n [x] = [x+n]
sumarN n (x:xs) = (x+n) : sumarN n xs

-- Ejercicio 3.5

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [] = []
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- Ejercicio 3.6

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [] = []
sumarElUltimo (x:xs) = sumarN  (head(reverso(x:xs))) (x:xs)

--Ejercicio 3.7

pares :: [Integer] -> [Integer]
pares [] = []
pares [x]
    | x `mod` 2 == 0 = [x]
    | otherwise = []
pares (x:xs)
    | esPar x = x : pares xs
    | otherwise = pares xs

esPar :: Integer -> Bool
esPar x = x `mod` 2 == 0

--Ejercicio 3.8

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)
    | x `mod ` n == 0 = x : multiplosDeN n xs
    | otherwise = multiplosDeN n xs

-- Ejercicio 3.9

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar lista = (ordenar(quitar (maximo lista)lista)) ++ [(maximo lista)]

-- Ejercicio 4.1

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) 
    | x == ' ' && y == ' ' = sacarBlancosRepetidos (y:xs)
    | otherwise = x : sacarBlancosRepetidos (y:xs)

-- Ejercicio 4.2

contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras (x:xs)
    | x == ' ' && (head (reverso (x:xs))) == ' ' = contarBlancos (sacarBlancosRepetidos (x:xs)) - 1
    | x == ' ' || (head (reverso (x:xs))) == ' ' = contarBlancos (sacarBlancosRepetidos (x:xs))
    | otherwise = contarBlancos (sacarBlancosRepetidos (x:xs)) + 1

contarBlancos :: [Char] -> Integer
contarBlancos [] = 0
contarBlancos (x:xs)
    | x == ' ' = 1 + contarBlancos xs
    | otherwise = contarBlancos xs

-- Ejercicio 4.3
palabras :: [Char] -> [[Char]]
palabras x = palabrasAux (dejarSinBlancos x) (dejarSinBlancos x) 

palabrasAux :: [Char] -> [Char] -> [[Char]]
palabrasAux [] l = [laPrimeraPalabra l]
palabrasAux (x:xs) l 
    | x == ' ' = laPrimeraPalabra l : palabrasAux xs xs
    | otherwise = palabrasAux xs l

laPrimeraPalabra :: [Char] -> [Char]
laPrimeraPalabra [] = []
laPrimeraPalabra (x:xs) 
    | x == ' ' = []
    | otherwise = x : laPrimeraPalabra xs

dejarSinBlancos :: [Char] -> [Char]
dejarSinBlancos [] = []
dejarSinBlancos (x:xs) = sacarPrimerBlanco(sacarUltimoBlanco(sacarBlancosRepetidos (x:xs)))

sacarPrimerBlanco :: [Char] -> [Char]
sacarPrimerBlanco (x:xs)
    | x == ' ' = xs
    | otherwise = (x:xs)

sacarUltimoBlanco :: [Char] -> [Char]
sacarUltimoBlanco (x:xs)
    | head (reverso (x:xs)) == ' ' = principio (x:xs)
    | otherwise = (x:xs)

-- Ejercicio 4.4

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga (x:y:xs) 
    | contarPalabras (x:y:xs) == 1 = dejarSinBlancos(x:y:xs)
    | cuentaCaracteres (head (palabras (x:y:xs))) >= cuentaCaracteres (head(palabras(y:xs))) = palabraMasLarga (x:xs)
    | otherwise = palabraMasLarga (y:xs)

cuentaCaracteres :: [Char] -> Integer
cuentaCaracteres [] = 0
cuentaCaracteres (x:xs) = 1 + cuentaCaracteres xs

comparaLargos :: [Char] -> [Char] -> [Char]
comparaLargos [] [] = []
comparaLargos [] l = l
comparaLargos l [] = l
comparaLargos (x:xs) (y:ys) 
    | cuentaCaracteres (x:xs) >= cuentaCaracteres (y:ys) = (x:xs)
    | otherwise = (y:ys)
