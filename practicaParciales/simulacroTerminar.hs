-- Ejercicio 4 del simulacro q no pude terminar

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs) = not (tieneComponentesIguales x) && not (laTuplaPertenece x xs) && relacionesValidas xs

tieneComponentesIguales :: (String, String) -> Bool
tieneComponentesIguales (x,y) = x == y 

laTuplaPertenece :: (String, String) -> [(String,String)] -> Bool
laTuplaPertenece _ [] = False
laTuplaPertenece (a,b) ((c,d):xs)
    | (a == c && b == d) || (b == c && a == d) = True
    | otherwise = laTuplaPertenece (a,b) xs



personas :: [(String, String)] -> [String]
personas [] = []
personas (x:xs) = fst x : snd x : []



amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe a ((c,d):xs) 
    | tieneUnAmigo a (c,d) = c : d : [] ++ amigosDe a xs
    | otherwise = amigosDe a xs 

tieneUnAmigo :: String -> (String,String) -> Bool
tieneUnAmigo x (a,b) 
    | x == a || x == b = True
    | otherwise = False


-- Ejercicio 4


personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [x] = fst x
personaConMasAmigos (x:xs) = tuplaMayor (tuplarPersonCantAmigos (personasComoTendriaQueSer(x:xs)))

tuplarPersonCantAmigos :: [String] -> [(String,Int)]
tuplarPersonCantAmigos [] = []
tuplarPersonCantAmigos (x:xs) = (x, (cantidadDeApariciones x (x:xs))) : tuplarPersonCantAmigos (eliminar x (x:xs))

cantidadDeApariciones :: String -> [String] -> Int
cantidadDeApariciones _ [] = 0
cantidadDeApariciones a (x:xs)
    | a == x = 1 + cantidadDeApariciones a xs
    | otherwise = cantidadDeApariciones a xs 


largo :: [String] -> Int
largo (x:xs) = 1 + largo xs

tuplaMayor :: [(String,Int)] -> String
tuplaMayor [(a,b)] = a 
tuplaMayor ((a,b):(c,d):xs)
    | b >= d = tuplaMayor ((a,b):xs)
    | otherwise = tuplaMayor ((c,d):xs)

eliminar :: String -> [String] -> [String]
eliminar _ [] = []
eliminar a (x:xs)
    | a == x = eliminar a xs 
    | otherwise = x : eliminar a xs

personasComoTendriaQueSer :: [(String, String)] -> [String] -- solo q sin sacar a los repetidos ya q lo necesito
personasComoTendriaQueSer [] = []
personasComoTendriaQueSer (x:xs) = destuplar x ++ personasComoTendriaQueSer xs

destuplar :: (String,String) -> [String]
destuplar (a,b) = a : b : []

{--

Enunciado
```
    Ejercicio 1
    Para empezar a diseñar la novedosa y rupturista red social Y el famoso Elio Mark nos ha pedido que desarrollemos algunas funciones básicas, que tendrán como objetido representar algunas relaciones e interacciones entre los usuarios. Para esto nos envió las siguientes especificaciones en lenguaje semiformal y nos pidió que hagamos el desarrollo enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que se ven en Introducción a la Programación de Exactas-UBA.

    problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
      requiere: {True}
      asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
    }
    1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.

    problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
      requiere: {relacionesValidas(relaciones)}
      asegura: {res no tiene elementos repetidos}
      asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
    }

    problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
      requiere: {relacionesValidas(relaciones)}
      asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
    }

    problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
      requiere: {relaciones no vacía}
      requiere: {relacionesValidas(relaciones)}
      asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
    }
```
    Ejercicio 2

    Conteste marcando la opción correcta. En el contexto de la programación funcional, se llama polimorfismo:
    Cuando una función puede invocarse con distintos tipos de datos sin tener que redefinirla.
    Cuando una función puede invocarse con distintos tipos de datos teniendo que definirla para cada tipo de dato particular.
    Cuando tengo un tipo de dato que puede ser usado para invocar a muchas funciones.
    --}