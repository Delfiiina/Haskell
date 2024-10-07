module TestTP where 

import test.HUnit
import Data.List
import Solucion 

-- Ejercicio 2
ciudadesConectadasTests =
    test [
    "Agencia de viajes tiene 1 elemento, ciudad no está" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires","Cordoba",1.5)] "Rosario")  [],
    "Agencia de viajes tiene 1 elemento, ciudad es origen" ~: expectPermutacion(ciudadesConectadas [("Buenos Aires","Cordoba",1.5)] "Buenos Aires")  ["Cordoba"],
    "Agencia de viajes tiene 1 elemento, ciudad es destino" ~: expectPermutacion(ciudadesConectadas [("Buenos Aires","Cordoba",1.5)] "Cordoba")  ["Buenos Aires"],
    "Agencia de viajes varios elementos, ciudad no está" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Cordoba", "Mendoza", 1.2),("Mendoza", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Chubut")  [],
    "y ciudad está 1 vez como origen, su destino no se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Chubut", "Mendoza", 1.2),("Mendoza", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Buenos Aires" )  ["Córdoba"],
    "y ciudad está 1 vez como origen, su destino se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Cordoba", "Mendoza", 1.2),("Mendoza", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Buenos Aires" )  ["Córdoba"],
    "y ciudad está 1 vez como destino, su origen no se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Chubut", "Mendoza", 1.2),("Tucuman", "Salta", 1.8),("Salta", "Tucuman", 0.5)] "Mendoza" )  ["Chubut"],
    "y ciudad está 1 vez como destino, su origen se repite en otro vuelo" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Chubut", "Mendoza", 1.2),("Tucuman", "Chubut", 1.8),("Salta", "Tucuman", 0.5)] "Mendoza" )  ["Chubut"],
    "y ciudad está más de una vez, solo como origen, y destinos no se repiten" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Buenos Aires", "Mendoza", 1.2),("Tucuman", "Chubut", 1.8),("Salta", "Tucuman", 0.5)] "Buenos Aires" )  ["Cordoba","Mendoza"],
    "y ciudad está más de una vez, solo como origen, y sus destinos se repiten" ~: expectPermutacion (ciudadesConectadas [("Buenos Aires", "Cordoba", 1.5),("Buenos Aires", "Mendoza", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Mendoza", 0.5)] "Buenos Aires" )  ["Cordoba","Mendoza"],
    "y ciudad está más de una vez, solo como destino, y sus origenes no se repiten" ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "Mendoza", 1.2),("Tucuman", "Cordoba", 1.8),("Salta", "Jujuy", 0.5)] "Cordoba" )  ["Santa Fe","Tucuman"],
    "y ciudad está más de una vez, solo como destino, y sus origenes se repiten" ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "Santa Fe", 1.2),("Tucuman", "Cordoba", 1.8),("Tucuman", "Jujuy", 0.5)] "Cordoba")  ["Santa Fe","Tucuman"],
    "y ciudad está más de una vez, mezcla de origen y destino, y sus destinos/origenes no se repiten" ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Mendoza", 1.2),("Cordoba", "Chubut", 1.8),("Salta", "Jujuy", 0.5) ] "Cordoba")  ["Samta Fe","Chubut","Mendoza"],
    "y ciudad está más de una vez, mezcla de origen y destino, y sus destinos/origenes se repiten " ~: expectPermutacion (ciudadesConectadas [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Cordoba")  ["Jujuy","Santa Fe"]
    ]


-- Ejercicio 5

sePuedeLlegarTests =
    test [
        "Agencia de viajes tiene 1 elemento" ~: (sePuedeLlegar [("Santa Fe","Buenos Aires",1.5)] "Chubut" "Santa Fe") ~?= False,
        "Agencia de viajes tiene más elementos, no hay niguna ruta y ciudades aparecen" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Tucuman" "Jujuy") ~?= False,
        "No hay niguna ruta ya que las ciudades no aparecen" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Salta" "La Pampa") ~?= False,
        "No hay ninguna ruta, pero el camino aparece al revés" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Cordoba", "Buenos Aires", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Jujuy" "Cordoba") ~?= False,
        "Hay una ruta directa" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "Santa Fe", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Santa Fe" "Cordoba") ~?= True,
        "Hay una sola ruta con escala" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "La Pampa", 1.2),("Tucuman", "Chubut", 1.8),("Cordoba", "Jujuy", 0.5)] "Santa Fe" "Jujuy") ~?= True,
        "Hay forma de llegar con distintas escalas" ~: (sePuedeLlegar  [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "La Pampa", 1.2),("Tucuman", "Buenos Aires", 1.8),("Cordoba", "Jujuy", 0.5),("Cordoba", "Buenos Aires", 2.5),("Santa Fe", "Tucuman", 4.5)] "Santa Fe" "Buenos Aires") ~?= True,
        "Hay escala y ruta directa al mismo tiempo" ~: (sePuedeLlegar [("Santa Fe", "Cordoba", 1.5),("Buenos Aires", "La Pampa", 1.2),("Tucuman", "Buenos Aires", 1.8),("Cordoba", "Jujuy", 0.5),("Cordoba", "Buenos Aires", 2.5),("Santa Fe", "Jujuy", 4.5),("Santa Fe", "Buenos Aires", 2.5)] "Santa Fe" "Buenos Aires") ~?= True
    ]