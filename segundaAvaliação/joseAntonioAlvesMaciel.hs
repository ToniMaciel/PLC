-- José Antônio Alves Maciel (jaam)

-- 01)
somaSqrt :: [Double] -> Double
somaSqrt = foldr (+) 0 . map sqrt . filter (> 0)

--02)
fatoresPrimos :: Integer -> Integer -> [Integer]
fatoresPrimos x y
 | x == 1       = []
 | (y^2) > x    = [x]
 | y == 2       = if (mod x 2 == 0) then 2 : fatoresPrimos (div x 2) 2 else fatoresPrimos x 3 
 | otherwise    = if (mod x y == 0) then y : fatoresPrimos (div x y) y else fatoresPrimos x (y+2) 

fatores :: Integer -> [Integer]
fatores n = foldr (\x seen -> if x `elem` seen then seen else x : seen) [] (fatoresPrimos n 2)

perfeitos :: Integer -> [Integer]
perfeitos num = [x | x <- reverse [2 .. num], x == (foldr (+) 0 . map (^2) . fatores) x] ++ [1]

-- 03)
unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' list = foldr (\(a, b) (c,d) -> (a:c, b:d)) ([], []) list

-- 04)
type Texto = String
type Id = String
type DataHoraPub = Int

data Post = Post (Id, DataHoraPub) Texto deriving (Show, Eq, Read)
data Thread = Nil | T Post (Thread) deriving (Read)

-- (a)
instance Show Thread where
    show Nil = ""
    show (T (Post (id, dataHoraPub) texto) thread) = "(" ++ id ++ " " ++ (show dataHoraPub) ++ " " ++ texto ++ ")" ++ show thread

-- (b)
inserirPost :: Post -> Thread -> Thread
inserirPost x thread = T (x) thread

--(c)
threadToList :: Thread -> [Post]
threadToList Nil = []
threadToList (T post thread) = post : threadToList thread

--(d)
listToThread :: [Post] -> Thread
listToThread [] = Nil
listToThread (x:xs) = T x (listToThread xs)

--(e)
postOwner :: (Id, DataHoraPub) -> Post -> Bool
postOwner x (Post y _) = (x == y)

removerPost :: (Id, DataHoraPub) -> Thread -> Thread
removerPost dadosPost thread = listToThread (filter (not . postOwner dadosPost) (threadToList thread))
