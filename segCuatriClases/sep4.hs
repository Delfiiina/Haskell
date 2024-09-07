type Punto2D = (Float,Float) -- No es necesario con mayúscula. Es buena práctica ponerlo arriba de todo
todoMenor :: Punto2D -> Punto2D -> Bool
todoMenor (a,b) (c,d) = a < c && b < d -- Se entiende todavia más si se pone (a1,a2) y (b1,b2)
-- f)
posPrimerPar :: (Int,Int,Int) -> Int
posPrimerPar (a,b,c) 
    | a `mod` 2 == 0 = 0
    | b `mod` 2 == 0 = 1
    | c `mod` 2 == 0 = 2
    | otherwise = 4
-- c)distanciaPuntos: calcula la distancia entre dos puntos de R2
distanciaPuntos :: Punto2D -> Punto2D -> Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
