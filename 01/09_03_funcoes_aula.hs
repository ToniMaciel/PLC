{- 
threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = not(m == n)  && not(n == p) && not(m==p)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p q = (m == n)  && (n == p) && (p == q)

fourEqual2 :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual2 m n p q = not (threeDifferent m n p) && not (threeDifferent m n q) &&
                     not (threeDifferent n p q) && not (threeDifferent m p q) -- ...


fat :: Integer -> Integer
fat n
  | n == 0 = 1
  | n > 0  = fat(n-1) * n
--fat 0 = 1
--fat n = fat (n-1) * n

{-
fat 3
= fat 2 * 3
= (fat 1 * 2) * 3
= ((fat 0 * 1) * 2) * 3
= ((1 * 1) * 2) * 3
= (1 * 2) * 3
= 2 * 3
= 6
-}

tailFat :: Integer -> Integer -> Integer
tailFat 0 x = x
tailFat n x = tailFat (n-1) (n*x)

fat' :: Integer -> Integer
fat' n = tailFat n 1

-- Lista de exercicios

--1
dobro :: Integer -> Integer
dobro n = 2 * n

--2
quadruplo :: Integer -> Integer
quadruplo n = dobro (dobro n)

{-
dobro (dobro 3)
= 2 * dobro 3
= 2 * 2 * 3
= 4 * 3
= 12
-}


--3
poli2 :: Double ->  Double ->  Double ->  Double ->  Double
poli2 a b c x = a*x*x + b*x + c

--4
parImpar :: Integer -> String
parImpar n 
 | ehPar n   = "par"
 | otherwise = "impar"

ehPar :: Integer -> Bool
ehPar n = mod n 2 == 0

parImpar2 :: Integer -> String
parImpar2 n = if ehPar n then "par" else "impar"

-- 5

maxi :: Integer -> Integer -> Integer
maxi m n
 | m >= n    = m
 | otherwise = n

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree m n p
 | m >= n && m >= p = m
 | n >= p           = n
 | otherwise        = p

maxFour :: Integer -> Integer -> Integer -> Integer -> Integer 
maxFour m n p q =  maxi (maxThree m n p) q

--maxi (maxi (maxi (m n) p)) q

--6

--7
ehZero :: Integer -> Bool
ehZero 0 = True
ehZero n = False

ehZero2 :: Integer -> Bool
ehZero2 0 = True
ehZero2 _ = False

--8
sumTo :: Integer -> Integer
sumTo 1 = 1
sumTo n = sumTo (n-1) + n

--9
{-
potencia 2 2
= 2 * potencia 2 1
= 2 * (2 * potencia 2 0)
= 2 * (2 * 1)
= 2 * 2
= 4 
-}

potencia :: Integer -> Integer -> Integer
potencia n 0 = 1
potencia n k = n * potencia n (k-1)

-- 10
binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

--11
tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci n = tribonacci' 1 1 2 (n-2)

tribonacci' :: Integer -> Integer -> Integer -> Integer -> Integer
tribonacci' a b c 1 = c
--tribonacci' a b c n = tribonacci' ....

--12
addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1) 

-- 13
paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str

-- 14

vendas 0 = 3
vendas 1 = 7
vendas 2 = 12 


imprimeTotal :: Int -> String
imprimeTotal n = paraDireita 3 ("Total" ++ addEspacos 3 ++ show(totalVendas n) ++ "\n")

imprimeTabela :: Int -> String
imprimeTabela n = cabecalho
                  ++ imprimeSemanas n
                  ++ imprimeTotal n
                  ++ imprimeMedia n

impressao :: Int -> IO()
impressao n = putStr (imprimeTabela n) 

-}

--1
dobro :: Integer -> Integer
dobro n = 2 * n

--2
quadruplo :: Integer -> Integer
quadruplo n = dobro (dobro n)

-- 3

poli2 :: Double -> Double -> Double -> Double -> Double
poli2 a b c x = a*x*x + b*x + c

-- 4
parImpar :: Integer -> String
parImpar n
 | ehPar' n   = "par"
 | otherwise = "impar"
 where
   ehPar' :: Integer -> Bool
   ehPar' n = n `mod` 2 == 0

ehPar :: Integer -> Bool
ehPar n = mod n 2 == 0

parImpar2 :: Integer -> String
parImpar2 n = if ehPar n then "par" else "impar"

--
f :: Integer -> Integer -> Integer
f a b  = a + b

{-
f (5+7) 3
= (5+7) + 3
= 12 + 3
= 15
-}

--11


fat :: Integer -> Integer
fat n
  | n == 0   = 1
  | n > 0    = fat (n-1) * n

{-
fat 3
= fat 2 * 3
= (fat 1 * 2) * 3
= ((fat 0 * 1)) * 2) * 3
= ((1 * 1) * 2) * 3
= (1* 2) * 3
= 2 * 3
= 6
-}

tailFat 0 x = x
tailFat n x = tailFat (n-1) (n*x)

fat' n = tailFat n 1

{-
fat'3
= tailFat 3 1
= tailFat 2 3
= tailFat 1 6
= tailFat 0 6
= 6
-}

{- tribonacci :: Integer -> Integer
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci n = tribonacci' 1 1 2 (n-2)

tribonacci' :: Integer -> Integer -> Integer -> Integer -> Integer 
tribonacci' a b c 1 = c
tribonacci' a b c n = tribonacci' ....  (n-1) -}

--12

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos(n-1)

--13
paraDireita :: Int -> String -> String
paraDireita n str = addEspacos n ++ str

--14

vendas 0 = 1
vendas 1 = 5
vendas 2 = 6
vendas 3 = 7

totalVendas :: Int -> Int
totalVendas 0 = vendas 0
totalVendas n = totalVendas(n-1) + vendas n

imprimeTotal :: Int -> String
imprimeTotal n = paraDireita 3 ("Total" 
                             ++ addEspacos 3 
                             ++ show (totalVendas n)
                             ++ "\n")

imprimeMedia :: Int -> String
imprimeMedia n = paraDireita 3 ("Media" ++ addEspacos 3 ++ 
             show( (fromIntegral(totalVendas n)) / (fromIntegral (n+1))) ++ "\n")

imprimeTabela :: Int -> String
imprimeTabela n = --cabecalho
                 -- ++ imprimeSemanas n
                  imprimeTotal n
                 ++ imprimeMedia n

impressao :: Int -> IO()
impressao n = putStr (imprimeTabela n)