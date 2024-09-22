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
    | pertenece x xs == False = [x] ++ xs
    | otherwise = x : (eliminarRepetidos (quitarTodos x xs))
    
-- Ejercicio 3.3

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) 
    | x >= y = maximo (x:xs)
    | otherwise = maximo (y:xs)

-- Ejercicio 3.9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar lista = (ordenar(quitar (maximo lista)lista)) ++ [(maximo lista)]