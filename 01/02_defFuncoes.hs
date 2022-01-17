quadrado :: Integer -> Integer
quadrado x = x * x

addD :: Integer -> Integer -> Integer 
addD n m = 2 * (n + m)

somaDobroQuadrado :: Int -> Int -> Int
somaDobroQuadrado x y = (2 * (x * x)) + (2 * (y * y))

somaDobroQuadrado2 :: Int -> Int -> Int
somaDobroQuadrado2 x y = dSqX + dSqY
   where
       dSqX = 2 * (x * x)
       dSqY = 2 * (y * y)

somaDobroQuadrado3 :: Int -> Int -> Int
somaDobroQuadrado3 x y = dSq x + dSq y
          where
              dSq m = 2 * (m * m)

somaDobroQuadrado4 :: Int -> Int -> Int
somaDobroQuadrado4 x y = dSq x + dSq y + f
     where
        dSq m = 2 * (m * m)
        f = 6

somaDobroQuadradoL :: Int -> Int -> Int
somaDobroQuadradoL x y = 
      let dSqX = 2 * (x * x)
          dSqY = 2 * (y * y)
      in dSqX + dSqY
               
fat :: Integer -> Integer
fat n
 | n == 0 = 1  -- Caso base
 | n > 0 = fat(n-1) * n -- Caso recursivo

fib :: Integer -> Integer
fib n
 | n == 0   = 0
 | n == 1   = 1
 | n > 1    = fib (n-2) + fib (n-1)

ehImpar :: Int -> Bool
ehImpar n 
 |  n<=0 = False
 |otherwise = ehPar(n-1)

ehPar :: Int -> Bool
ehPar n 
 | n<0 = False
 | n==0 = True
 | otherwise = ehImpar(n-1)

{-vendas :: Int -> Int
vendas 0 = 2
vendas 1 = 5
vendas 2 = 7
vendas 3 = 3

totalVendas :: Int -> Int
totalVendas n
 | n == 0    = vendas 0
 | otherwise = totalVendas (n-1)  +  vendas n

totalVendasCP :: Int -> Int
totalVendasCP 0 = vendas 0
totalVendasCP n = totalVendasCP (n-1)  +  vendas n-}

mOr :: Bool -> Bool -> Bool
mOr True True = True
mOr True False = True
mOr False True = True
mOr False False = False

mOr2 :: Bool -> Bool -> Bool
mOr2 True x = True
mOr2 False x = x

mOr3 :: Bool -> Bool -> Bool
mOr3 True _ = True
mOr3 False x = x

tailFat :: Integer -> Integer -> Integer
tailFat 0 x = x
tailFat n x = tailFat  (n - 1) (n*x)
fat' n = tailFat n 1
{-
tailFat 3 1
= tailFat 2 3
= tailFat 1 6
= tailFat 0 6
= 6

-}

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) (max b c)

double :: Integer -> Integer
double x = 2 * x

quadruplo :: Integer -> Integer
quadruplo x = double (double x)

quadradoD :: Double -> Double
quadradoD x = x * x

poli :: Double -> Double -> Double -> Double -> Double
poli a b c x = (a * (quadradoD x) + b * x) + c

ehParToni :: Integer -> Bool
ehParToni a = if (mod a 2) == 0 then True else False

parImparToni :: Integer -> String
parImparToni a = if ehParToni a then "par" else "impar"

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour a b c d
 | a > b && a > c && a > d   = a
 | b > c && b > d            = b
 | c > d                     = c
 | otherwise                 = d

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d = max (max a b) (max c d)

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3 a b c d = max a (maxThree b c d)

isEqual :: Integer -> Integer -> Int
isEqual a b = if a == b then 1 else 0

quantosIguais :: Integer -> Integer -> Integer -> Int
quantosIguais a b c 
 | ((isEqual a b) + (isEqual a c)) + isEqual b c == 1 = 2
 | otherwise = ((isEqual a b) + (isEqual a c)) + isEqual b c

ehZero :: Integer -> Bool
ehZero 0 = True
ehZero n = False

sumTo :: Integer -> Integer
sumTo n
 | n == 1 = 1
 | otherwise = n + sumTo (n - 1)

potencia :: Integer -> Integer -> Integer
potencia a b
 | b == 1 = a
 | otherwise = a * potencia a (b - 1)

b1 :: Integer -> Integer -> Integer
b1 n 0 = 1
b1 0 k = 0
b1 n k = b1 (n - 1) k + b1 (n - 1)(k -1)

tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci n = tribonacciAux 1 1 2 (n-2)

tribonacciAux :: Integer -> Integer -> Integer -> Integer -> Integer
tribonacciAux a b c n
 | n == 1 = c
 | otherwise = tribonacciAux b c (a + b + c) (n-1)

addEspacos :: Int -> String
addEspacos 1 = " "
addEspacos n = " " ++ addEspacos (n - 1)

paraDireita :: Int -> String -> String
paraDireita n s = addEspacos n ++ s

vendas :: Int -> Int
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15
vendas 3 = 20

cabecalho = "Semana" ++ paraDireita 2 "Venda" ++ "\n"

pegaValor :: Int -> Int -> String
pegaValor a b
 | a == b = paraDireita 2 (show a) ++ paraDireita 5 (show (vendas a) ++ "\n")
 | otherwise = paraDireita 2 (show a) ++ paraDireita 5 (show (vendas a) ++ "\n") ++ pegaValor (a+1) b 

imprimeSemanas :: Int -> String
imprimeSemanas n = pegaValor 0 n

somaTotal :: Int -> Int
somaTotal 0 = vendas 0
somaTotal n = vendas n + somaTotal (n - 1) 

imprimeTotal :: Int -> String
imprimeTotal n = "Vendas" ++ paraDireita 2 (show (somaTotal n) ++ "\n")

imprimeMedia :: Int -> String
imprimeMedia n = "Média" ++ paraDireita 3 (show (fromIntegral(somaTotal n)/fromIntegral(n + 1))) ++ "\n"

imprimeTabela :: Int -> String
imprimeTabela n = cabecalho
                  ++ imprimeSemanas n
                  ++ imprimeTotal n
                  ++ imprimeMedia n

impressao :: Int -> IO()
impressao n = putStr (imprimeTabela n)