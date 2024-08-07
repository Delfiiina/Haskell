saludo x = "Hola! soy " ++ x

numerosSimples x = if x >= 5 then  x - 5  else x

pruebaSentenciaIf x = 
    if x `mod` 2 == 0 
        then "Ese numero es par!"
    else "Ese numero es impar!"

usoDeHead x = head x 
usoDeTail x = tail x
--
soloPares xs = [x | x <- xs , x `mod` 2 == 0 ] 

usoDeLast :: Integer -> [Integer] -> Bool
usoDeLast x (y:ys)
    | last (y:ys) == x = True
    | otherwise = False

circumference :: Float -> Float
circumference r = 2 * pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r
