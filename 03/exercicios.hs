--01)

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv x y
 | y == 0 = Nothing
 | otherwise = Just (div x y)

--02)

safeDivAlt :: Integer -> Integer -> Either String Integer
safeDivAlt x y
 | y == 0 = Left (show x ++ "/0")
 | otherwise = Right (div x y)

--03)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f lst = [ x | Just x <- (map f lst)]

--04)

classifica :: [Either a b] -> ([a], [b])
classifica lst = ([x | Left x <- lst],[y | Right y <- lst])

--05)a)

data Tree a = Leaf | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

valAtRoot :: Tree a -> Maybe a
valAtRoot Leaf = Nothing
valAtRoot (Node x _ _ ) = Just x

--05)b)

depth :: Tree a -> Integer
depth Leaf = 0
depth (Node x leftNode rightNode) = 1 + max (depth leftNode) (depth rightNode)

--05)c)

leftValue :: Tree a -> Maybe a
leftValue Leaf = Nothing
leftValue (Node x Leaf _) = Just x
leftValue (Node x leftNode _) = leftValue leftNode

--05)d)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree func Leaf = Leaf
mapTree func (Node x leftNode rightNode) = Node (func x) (mapTree func leftNode) (mapTree func rightNode)

--05)e)

insertL :: Integer -> Tree Integer -> Tree Integer
insertL x Leaf = (Node x Leaf Leaf)
insertL int (Node x leftNode rightNode) = Node x (insertL int leftNode) rightNode

--05)f)

medida :: Tree a -> Tree Integer
medida Leaf = Leaf
medida (Node _ leftNode rightNode) = (Node (1 + (max (depth leftNode) (depth rightNode))) (medida leftNode) (medida rightNode))

--05)g)

tripleSum :: Integer -> Integer -> Integer -> Integer
tripleSum a b c = a + b + c

tripleConcat :: Char -> String -> String -> String
tripleConcat a b c = a : b ++ c

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree f x Leaf = x
foldTree f x (Node a lf rg) = f a (foldTree f x lf) (foldTree f x rg)

--05)h)

treeSum :: Tree Integer -> Integer
treeSum x = foldTree (tripleSum) 0 x