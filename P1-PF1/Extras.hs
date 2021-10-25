import Test.QuickCheck

-------------------------------------------------------------------------------
-- Ejercicio 1
-------------------------------------------------------------------------------

esPrimo :: Integer -> Bool
esPrimo n | n <= 0 = error "Argumento negativo o cero"
          | otherwise = length(divisores n) == 2

divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n],n `mod` x==0 ]

-------------------------------------------------------------------------------
-- Ejercicio 2
-------------------------------------------------------------------------------

libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n | n <= 0 = error "Argumento negativo o cero"
                   | n == 1 = True
                   | otherwise = libre' n 2


libre' :: Integer -> Integer -> Bool
libre' n p | n>=p = if(n `mod` p^2 == 0) then False else libre' n (p+1)
                    | otherwise = True

sumaDigitos :: Integer -> Integer
sumaDigitos n | n < 0 = error "Argumento Negativo"
              | n < 10 = n
              | otherwise = mod n 10 + sumaDigitos (div n 10)

-------------------------------------------------------------------------------
-- Ejercicio 3
-------------------------------------------------------------------------------

harshad :: Integer -> Bool
harshad n | n <= 0 = error "Argumento No Positivo"
          | otherwise = n `mod` sumaDigitos n == 0

harshadMultiple :: Integer -> Bool
harshadMultiple n | n <= 0 = error "Argumento No Positivo"
                  | otherwise = harshad n &&  harshad (div n (sumaDigitos n))

vecesHarshad :: Integer -> Integer
vecesHarshad n | n <= 0 = error "Argumento No Positivo"
               | harshad n == False = 0 
               | otherwise = harshad' n 0

harshad' :: Integer -> Integer -> Integer
harshad' n c | harshadMultiple n && n > 1 = harshad' (div n (sumaDigitos n)) (c+1)
             | otherwise = (c+2)
             
-------------------------------------------------------------------------------
-- Ejercicio 4
-------------------------------------------------------------------------------

fib :: Integer -> Integer
fib n | n <  0    = error("Argumento Negativo") 
      | n == 0    = 0 
      | n == 1    = 1
      | otherwise = fib (n-1) + fib (n-2)

llamadasFib :: Integer -> Integer
llamadasFib n | n <  0    = error("Argumento Negativo")
              | n <= 1    = 1
              | otherwise = 1 + llamadasFib(n-1) + llamadasFib(n-2)

fib' :: Integer -> Integer
fib' n = fibAc n 0 1
  where fibAc n x y | n  < 0    = error("Argumento Negativo")
                    | n == 0    = x
                    | n == 1    = y
                    | otherwise = fibAc (n-1) y (x+y)

binet :: Integer -> Integer
binet n | n  < 0     = error ("Argumento Negativo")
        | n == 0    = 0 
        | n == 1    = 1
        | otherwise =  round((aureo^n - (1 - aureo)^n)/sqrt 5)
  where 
   aureo = (1 + sqrt 5) / 2

p_fib n = n >= 0 && n <= 30 ==> fib n == fib' n 

p_binet n = n>= 0 && n < 76 ==> binet n == fib' n 

-- A partir del valor 76 la aproximacion de binet falla ese es el valor limite
