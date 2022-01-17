example :: Integer
example = double (size - square (2+2))

doubleSquare :: Integer -> Integer
doubleSquare n = square (double n)

squareDouble :: Integer -> Integer
squareDouble n = double (square n)

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent a b c = (a /= b) && (a /= c) && (b /= c)

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual a b c = (a == b) && (a == c)

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual a b c d = (a == b) && (a == c) && (a == d)

fourEqual1 :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual1 a b c d = ( threeEqual a b c ) && (a == d)

max' :: Integer -> Integer -> Integer
max' x y
 | x >= y = x
 | otherwise = y

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) (max b c)

min' :: Integer -> Integer -> Integer
min' x y
 | x > y = y
 | otherwise = x

minThree :: Integer -> Integer -> Integer -> Integer
minThree a b c = min (min a b) (min b c)
