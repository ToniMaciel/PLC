import Data.List

-- 01)

checkLists :: (Eq a, Show a) => [a] -> [a] -> Maybe String
checkLists [] [] = Nothing
checkLists (x:xs) (y:ys) = if (x /= y) then Just (show (x) ++ " /= " ++ show (y)) else checkLists xs ys   

findDifference :: (Eq a, Show a) => [a] -> [a] -> Maybe String
findDifference lstA lstB
 | length (lstA) == length (lstB) =  checkLists lstA lstB
 | otherwise = Just (show (length lstA) ++ " /= " ++ show (length lstB))

-- 02) 

data Vetor = Vetor Integer Integer Integer
    deriving Show

instance Eq Vetor where
    Vetor a b c == Vetor x y z = (a == x) && (b == y) && (c == z)

-- 03)

instance Num Vetor where
    Vetor a b c + Vetor x y z = Vetor (x+a) (b+y) (c+z)
    Vetor a b c - Vetor x y z = Vetor (a-x) (b-y) (c-z)
    Vetor a b c * Vetor x y z = Vetor (x*a) (b*y) (c*z)
    negate (Vetor a b c) = Vetor (negate a) (negate b) (negate c)
    abs (Vetor a b c) = Vetor (abs a) (abs b) (abs c)
    signum (Vetor a b c) = Vetor (signum a) (signum b) (signum c)

-- 04)

freqs :: Eq a => [a] -> [(Int, a)]
freqs lst = [(a, x) | x <- nub (lst), let a = length (elemIndices x lst)]

-- 05)

data ITree = ILeaf | INode Int ITree ITree
    deriving Show

instance Eq ITree where
    ILeaf == ILeaf = True
    ILeaf == _ = False
    INode _ _ _ == ILeaf = False
    INode i treeA treeB == INode j treeC treeD = (i == j) && (treeA == treeC) && (treeB == treeD)