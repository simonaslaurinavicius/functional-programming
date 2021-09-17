{- FirstScript.hs -}
module FirstScript where

-- The value size is an integer
size :: Integer
size = 25

-- The function to square an integer
square :: Integer -> Integer
square n = n*n

-- The function to double an integer
double :: Integer -> Integer
double n = 2*n

-- An example using double, square, and size
example :: Integer
example = double (size - square (2+2))

-- Check if a parameter is equal to the value of 'size'
equalSize :: Integer -> Bool
equalSize x = mod x size == 0

size2 :: Integer
size2 = 32

-- Check if a parameter is equal to the value of 'size2' (no syntactic sugar)
equalSize2 :: Integer -> Bool
equalSize2 = \x -> x `mod` size2 == 0
