-- José Antônio Alves Maciel (jaam)

-- 01)
exponencial :: Integer -> Integer -> Integer
exponencial x y
 | y == 0 = 1
 | otherwise = x * (exponencial x (y - 1))

-- 02)
-- Assumindo que Y > 1, visto que se fosse igual, a resposta seria inifinito.
numDiv :: Integral a => a -> a -> a
numDiv x y
 | mod x y == 0 = 1 + numDiv (div x y) y
 | otherwise = 0

-- 03)
estaNaLista :: Integer -> [Integer] -> Bool
estaNaLista _ [] = False
estaNaLista a (x:xs)
 | a == x = True
 | otherwise = False || estaNaLista a xs

unicosAux :: [Integer] -> [Integer] -> [Integer]
unicosAux [] _ = []
unicosAux (x:xs) elementosRepetidos
 | estaNaLista x elementosRepetidos = unicosAux xs elementosRepetidos
 | estaNaLista x xs = unicosAux xs (x:elementosRepetidos)
 | otherwise = x : unicosAux xs elementosRepetidos

unicos :: [Integer] -> [Integer]
unicos lst = unicosAux lst []

-- 04)
-- Implementação do length
tamanhoLista :: [a] -> Int
tamanhoLista [] = 0
tamanhoLista (x:xs) = 1 + (tamanhoLista xs)

getElementos :: Int -> Int -> [a] -> ([a], [a])
getElementos a b lst 
 | a /= b = ((head lst) : x, xs)
 | otherwise = ([], tail lst)
 where
    (x, xs) = getElementos (a+1) b (tail lst)

remDiv :: Int -> [a] -> ([a], [a])
remDiv n lst 
 | n < 0  = ([], [])
 | n == 0 = ([], lst)
 | n < len = getElementos 0 (n-1) lst
 | n == len = (init lst, [])
 | n > len = (lst, [])
 where 
    len = tamanhoLista lst

--remDiv i lst = ([x | index <- [0 .. (i-2)], let x = lst !! index], [x | index <- [i .. (length lst) - 1], let x = lst !! index])

-- 05)
--(a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] lst = lst
merge lst [] = lst
merge (x:xs) (y:ys) = if x < y then x : (merge xs (y:ys)) else y : (merge (x:xs) ys)

--(c)
metade :: [a] -> ([a], [a])
metade [] = ([], [])
metade [x] = ([x], [])
metade (x:x1:xs) = (x:left, x1:right)
 where
     (left, right) = metade xs

--(b)
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort (x:xs) = merge (msort left) (msort right)
 where
     (left, right) = metade (x:xs)