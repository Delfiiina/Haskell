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
mismosElementos (x:xs) (y:ys) 