module TestsDeMisFunciones where


import Test.HUnit
import MisFunciones
import Data.List

run = runTestTT testsFibo
testsFibo = test [
    "Caso base 1: fib 0" ~: (fib 0) ~?= 0,
    "Caso base 2: fib 1" ~: (fib 1) ~?= 1,
    "Caso recursivo" ~: (fib 2) ~?= 1,
    "Otro caso recursivo" ~: (fib 3) ~?= 2   
    ]

run2 = runTestTT testsSuma
testsSuma = test [
    "Suma con 0" ~: (suma 1 0) ~?= 1,
    "Otra suma con 0" ~: (suma 0 2) ~?= 2,
    "Suma con números distintos de 0" ~: (suma 2 4) ~?= 6
    ]

run3 = runTestTT testPares
testPares = test [
    "Test lista vacia"  ~: (pares []) ~?= [],
    "Test lista sin pares" ~: (pares [1,3,5]) ~?= [],
    "Test lista con pares sin repetir" ~: expectPermutacion (pares [2,4,6]) [2,4,6],
    "Test lista con pares repetidos" ~: expectPermutacion (pares[2,4,6,4,6]) [2,4,6],
    "Test lista con pares e impares sin repetir" ~: expectPermutacion (pares [2,3,4,5,6]) [2,4,6],
    "Test lista con pares e impares repetidos" ~: expectPermutacion (pares [2,2,3,3,4,4,5,5,6,6]) [2,4,6]
    ]

-- Funciones extra

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)


