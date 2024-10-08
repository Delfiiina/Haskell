type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]


-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = [] -- Caso base 
ciudadesConectadas [(c1,d1,t1)] ciudad  -- Caso donde en AgenciaDeViajes solo hay 1 elemento
    | ciudad == c1 = [d1]  -- Si la ciudad es la de origen me da el destino
    | ciudad == d1 = [c1]  -- Si la ciudad es la de destino me da la de origen
    | otherwise = [] -- Si la ciudad no está conectada no devuelve ninguna 
ciudadesConectadas ((c1,d1,t1): vuelos) ciudad -- Caso donde AgenciaDeViajes tiene más de 1 elemento
    | ciudad == c1 = sacarCiudadesRepetidas([d1] ++ ciudadesConectadas vuelos ciudad)
    | ciudad == d1 = sacarCiudadesRepetidas([c1] ++ ciudadesConectadas vuelos ciudad)
    | otherwise = ciudadesConectadas vuelos ciudad 

sacarCiudadesRepetidas :: [Ciudad] -> [Ciudad]
sacarCiudadesRepetidas [] = []
sacarCiudadesRepetidas [x] = [x]
sacarCiudadesRepetidas (x:xs)
    | not (pertenece1 x xs) = [x] ++ sacarCiudadesRepetidas xs
    | otherwise = x : (sacarCiudadesRepetidas (sacarCiudadEspecifica x xs)) 

pertenece1 :: Ciudad -> [Ciudad] -> Bool -- Función que se fija si la ciudad pertenece a la lista de ciudades
pertenece1 _ [] = False
pertenece1 x (y:ys) = x == y || pertenece1 x ys

sacarCiudadEspecifica :: Ciudad -> [Ciudad] -> [Ciudad]
sacarCiudadEspecifica _ [] = []
sacarCiudadEspecifica x (y:ys)
    | x == y = sacarCiudadEspecifica x ys
    | otherwise = y : (sacarCiudadEspecifica x ys)
 
-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ = False
sePuedeLlegar ((c1,d1,t1):xs) origen destino = conUnViaje ((c1,d1,t1):xs) origen destino || conEscala ((c1,d1,t1):xs) origen destino

conUnViaje :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
conUnViaje [] _ _ = False
conUnViaje ((c1,d1,t1):xs) origen destino 
    | origen == c1 && destino == d1 = True
    | otherwise = conUnViaje xs origen destino 

conEscala :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
conEscala [] _ _ = False
conEscala ((c1,d1,t1):xs) origen destino = 
     (origen == (mostrarOrigen (chequeoOrigen ((c1,d1,t1):xs) origen))) &&
     (destino == (mostrarDestino(chequeoDestino ((c1,d1,t1):xs) destino))) && 
     (coinciden (chequeoOrigen ((c1,d1,t1):xs) origen) (chequeoDestino ((c1,d1,t1):xs) destino))

chequeoOrigen :: AgenciaDeViajes -> Ciudad -> Vuelo
--chequeoOrigen [] _ = ?? puedo hacer que primero chequee si están las ciudades en AdV y recién ahí chequee entonces nunca llegaria a este caso base 
chequeoOrigen ((c1,d1,t1):xs) origen 
    | origen == c1 = (c1,d1,t1)
    | otherwise = chequeoOrigen xs origen

chequeoDestino :: AgenciaDeViajes -> Ciudad -> Vuelo
--chequeoDestino [] _ = ??
chequeoDestino ((c1,d1,t1):xs) destino 
    | destino == d1 = (c1,d1,t1)
    | otherwise = chequeoDestino xs destino

coinciden :: Vuelo -> Vuelo -> Bool
coinciden (c1,d1,t1) (c2,d2,t2) = d1 == c2

mostrarOrigen :: Vuelo -> Ciudad
mostrarOrigen (c1,d1,t1) = c1

mostrarDestino :: Vuelo -> Ciudad
mostrarDestino (c1,d1,t1) = d1