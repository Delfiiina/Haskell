-- Ejercicio 1

aproboMasDeNMaterias :: [(String,[Int])] -> String -> Int -> Bool
aproboMasDeNMaterias ((a,b):xs) nombre n = recuentoDeMateriasAprobadas (buscarAlumno ((a,b):xs) nombre) > n 

buscarAlumno :: [(String,[Int])] -> String -> (String, [Int])
buscarAlumno [x] _ = x
buscarAlumno ((a,b):xs) nombre
    | nombre == a = (a,b)
    | otherwise = buscarAlumno xs nombre

recuentoDeMateriasAprobadas :: (String,[Int]) -> Int
recuentoDeMateriasAprobadas (a,(x:xs)) = sumarNotasAprobadas (x:xs)

sumarNotasAprobadas :: [Int] -> Int
sumarNotasAprobadas [] = 0
sumarNotasAprobadas (x:xs) 
    | x >= 4 = 1 + sumarNotasAprobadas xs
    | otherwise = sumarNotasAprobadas xs

-- Ejercicio 2

buenosAlumnos :: [(String, [Int])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos ((a,b):xs)
    | promedio (a,b) >= 8 && noTieneAplazos (a,b) = a : buenosAlumnos xs
    | otherwise = buenosAlumnos xs

promedio :: (String,[Int]) -> Float
promedio (a,(x:xs)) = (fromIntegral (sumarNotas (x:xs))) / (fromIntegral (cantTotalDeNotas (x:xs)))

sumarNotas :: [Int] -> Int
sumarNotas [] = 0
sumarNotas (x:xs) = x + sumarNotas xs

cantTotalDeNotas :: [Int] -> Int
cantTotalDeNotas [] = 0
cantTotalDeNotas (x:xs) = 1 + cantTotalDeNotas xs

noTieneAplazos :: (String,[Int]) -> Bool
noTieneAplazos ( _ , []) = True
noTieneAplazos (a,(x:xs)) = x >= 4 && noTieneAplazos (a, (xs))

-- Ejercicio 3

mejorPromedio :: [(String,[Int])] -> [Char]
mejorPromedio [x] = fst x
mejorPromedio ((a,b):(c,d):xs)
    | promedio (a,b) >= promedio (c,d) = mejorPromedio ((a,b):xs)
    | otherwise = mejorPromedio ((c,d):xs)

-- Ejercicio 4

seGraduoConHonores :: [(String,[Int])] -> Int -> [Char] -> Bool
seGraduoConHonores registro materias alumno =  (aproboMasDeNMaterias (registro) (alumno) (materias - 1)) && (pertenece (alumno) (buenosAlumnos (registro))) && (promedioSatisfactorio (registro) (alumno)) 

promedioSatisfactorio :: [(String,[Int])] -> [Char] -> Bool
promedioSatisfactorio registro alumno = (((promedio (buscarAlumno registro (mejorPromedio registro))) - (promedio (buscarAlumno (registro) (alumno)))) < 1)

pertenece :: [Char] -> [String] -> Bool
pertenece _ [] = False
pertenece n (x:xs)
    | n == x = True
    | otherwise = pertenece n xs
