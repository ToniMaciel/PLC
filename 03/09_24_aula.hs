

tamLista :: [a] -> Int
tamLista []      = 0
tamLista (x:xs)  = 1 + tamLista xs

{-
case exp of
    pdr1 -> exp1 ;
    pdr2 -> exp2

-}

-- data Maybe a = Nothing | Just a 

hd :: a -> [a] -> a
hd y (x:_)  = x
hd y []     = y


hdListInt :: [Int] -> Int
hdListInt l = case (hd (-9) l) of
                (-9)  -> (-9) ;
                 n    ->  n

hdMaybe :: [a] -> Maybe a
hdMaybe []     = Nothing
hdMaybe (x:_)  = Just x

hdListIntMaybe :: [Int] -> String
hdListIntMaybe l = case (hdMaybe l) of
                    Nothing ->  "Lista vazia"
                    Just n  ->  show n

data ListaInt = NilLI | ConsLI Int (ListaInt)
data Lista t = Nil | Cons t (Lista t)
   deriving (Show, Eq)

data Estacao = Pri | Ver | Out | Inv
     deriving (Show, Eq, Ord, Enum)

-- 1. safeDiv

safeDiv :: Integer -> Integer -> Maybe Integer
--safeDiv x 0    =  Nothing
--safeDiv x y    =  Just (div x y)
safeDiv x y
 | y == 0    = Nothing
 | otherwise = Just (div x y)


-- 2. 

-- data Either a b = Left a | Right b

eitherDiv :: Integer -> Integer -> Either String Integer
eitherDiv x 0 = Left $ show x ++ "/0"
eitherDiv x y = Right $ div x y


{- case (eitherDiv m n) of
    Left  x -> "Divisao por zero" ;
    Right y -> "Resultado da divisao: " ++ show y  -}

-- 4. classifica

classifica :: [Either a b] -> ([a], [b])
classifica es = go es [] []
  where
      go (Left a: es) as bs  = go es (a:as) bs
      go (Right b: es) as bs = go es as (b:bs)
      go [] as bs = (reverse as, reverse bs)
-- 5

data Tree a = Leaf | Node a (Tree a) (Tree a)
   deriving (Show, Eq)

-- 5(a)
valAtRoot :: Tree a -> Maybe a
valAtRoot Leaf = Nothing
valAtRoot (Node x _ _) = Just x

-- 5 (b)

tamArv :: Tree a -> Int
tamArv (Leaf) = 0
tamArv (Node _ l r) = 1 + tamArv l + tamArv r

-- 5 (c)

leftest :: Tree a -> Maybe a
leftest Leaf            = Nothing
leftest (Node x Leaf _) = Just x
leftest (Node _ l _)    = leftest l
