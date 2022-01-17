maiorTres :: Integer -> Integer -> Integer -> Integer
maiorTres a b c
 | a > b && a > c = a
 | b > c = b
 | otherwise = c

menorTres a b c
 | a <= b && a <=c = a
 | b <= c = b
 | otherwise = c

menorMaior :: Integer -> Integer -> Integer -> (Integer, Integer)
menorMaior a b c = (menorTres a b c, maiorTres a b c)

entre :: Integer -> Integer -> Integer -> Bool
entre a b c 
 | (b <= a && b >= c) || (b >= a && b <= c) = True
 | otherwise = False

valorMeio ::  Integer -> Integer -> Integer -> Integer
valorMeio  a b c
 | entre b a c = a
 | entre a b c = b
 | otherwise = c

ordenaTripla :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ordenaTripla (a, b, c) = (menorTres a b c, valorMeio a b c, maiorTres a b c)

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

xPonto :: Ponto -> Float
xPonto (x, y) = x

yPonto :: Ponto -> Float
yPonto (x, y) = y

ehVertical :: Reta -> Bool
ehVertical ((p1, p2),(p3, p4))
 | p1 == p3 = True
 | otherwise = False

pontoY :: Float -> Reta -> Float
pontoY x ((x1, y1),(x2, y2)) = (((y2 - y1)/(x2 - x1))*(x - x1))+y1

ehLetra :: Char -> Bool
ehLetra a = ('A' <= a) && (a <= 'z')

maiuscula :: Char -> Char
maiuscula a
 | fromEnum a >= 97 = toEnum ((fromEnum a) - 32)
 | otherwise = a

paraMaiuscula :: String -> String
paraMaiuscula str = [maiuscula a | a <- str, ehLetra a]

ehDivisor :: Integer -> Integer -> Bool
ehDivisor a b = (mod a b) == 0

divisores :: Integer -> [Integer]
divisores a
 | a > 0 = [b | b <- [1..a], ehDivisor a b]
 | otherwise = []

isPrime :: Integer -> Bool
isPrime a
 | a == 1 = False
 | length (divisores a) == 2 = True
 | otherwise = False

menorLista :: [Int] -> Int
menorLista [] = maxBound :: Int
menorLista [x] = x
menorLista (x:xs)
 | x <= restoLista = x
 | otherwise = restoLista
 where
     restoLista = menorLista xs

fibStep :: (Integer, Integer) -> (Integer, Integer)
fibStep (u, v) = (v, u + v)

fibPair :: Integer -> (Integer, Integer)
fibPair n
 | n == 0 = (0, 1)
 | otherwise = fibStep (fibPair (n - 1))

fastFib :: Integer -> Integer
fastFib n = fst (fibPair n)

auxFib :: Integer -> Integer -> String
auxFib a n
 | a == n = (show n) ++ "\t" ++ (show (fastFib n)) ++ "\n"
 | otherwise = (show a) ++ "\t" ++ (show (fastFib a)) ++ "\n" ++ (auxFib (a+1) n)

cabecalho = "n" ++ "\t" ++ "fib n" ++ "\n"

fibTable :: Integer -> String
fibTable n = cabecalho
             ++ auxFib 0 n

impressao :: Integer -> IO()
impressao n = putStr (fibTable n)

measure :: [t] -> Int
measure n = if null n then -1 else length n

takeFinal :: Int -> [t] -> [t]
takeFinal n lst = snd (splitAt ((length lst) - n) lst)

remove :: Int -> [t] -> [t]
remove n lst = (take n lst) ++ (drop (n+1) lst)

ehDigito :: Char -> Bool
ehDigito x = ('0' <= x) && ('9' >= x)

primeiroInteiro :: [Integer] -> Integer
primeiroInteiro [] = 0
primeiroInteiro (x:xs) = x + 1

primeiroInteiro1 :: [Integer] -> Integer
primeiroInteiro1 lst = if null lst then 0 else 1 + head lst

primeiroInteiro2 :: [Integer] -> Integer
primeiroInteiro2 lst = if null lst then 0 else head lst

doisPrimeiros :: [Integer] -> Integer
doisPrimeiros [] = 0
doisPrimeiros [x] = x
doisPrimeiros (x:xs) = x + head xs

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

doisPrimeiros1 :: [Integer] -> Integer
doisPrimeiros1 lst = if length lst > 2 then sumList (take 2 lst) else primeiroInteiro2 lst

produto :: [Integer] -> Integer
produto [] = 1
produto (x:xs) = x * produto xs

howManyEquals :: [Integer] -> Integer -> Integer
howManyEquals [] a = 0
howManyEquals (x:xs) a
 | a == x = 1 + howManyEquals xs a
 | otherwise = howManyEquals xs a 

unique :: [Integer] -> [Integer]
unique lst = [a | a <- lst, (howManyEquals lst a) == 1]

emOrdemCres :: [Integer] -> Bool
emOrdemCres [] = True
emOrdemCres [x, y] = (x <= y)
emOrdemCres (y:x:xs) = (y <= x) && emOrdemCres (x:xs)
