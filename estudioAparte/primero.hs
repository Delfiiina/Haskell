saludo x = "Hola! soy " ++ x

numerosSimples x = if x >= 5 then  x - 5  else x

pruebaSentenciaIf x = 
    if x `mod` 2 == 0 
        then "Ese numero es par!"
    else "Ese numero es impar!"

usoDeHead x = head x 
usoDeTail x = tail x
