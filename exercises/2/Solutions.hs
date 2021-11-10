{-
  Solutions.hs

  Solutions to the Problem set 2 for the
  Functional Programming course at Vilnius University

  Author: Simonas Laurinavicius
  Study program, course, group: Informatics, 4, 1
  Email: simonas.laurinavicius@mif.stud.vu.lt
  Last updated: 2021-11-10
-}

module Solutions where

import Data.List (sort)
import Data.Char (toUpper)

{-
  Exercise 1

    Define a function average :: [Float] -> Float
    which for a given number list returns their average value.
-}

average :: [Float] -> Float
average [] = 0
average xs = (sum xs) / fromIntegral (length xs)

{-
  sum :: (Foldable t, Num a) => t a -> a, thus
  it returns a Float for a Float list.

  length :: Foldable t => t a -> Int, thus it returns an Int, which is
  from Integral type class

  (/) :: Fractional a => a -> a -> a, Int (an instance of the type class
  Integral) is not inside Fractional type class, so it must be converted using
  fromIntegral :: (Integral a, Num b) => a -> b, which converts it to be inside
  Fractional type class.
-}


{-
  Exercise 2

    Write a function divides :: Integer -> [Integer],

    which for any integer number returns a list of its divisors.
    Create two versions of such a function,
    one based on recursion and the other one on the list comprehension method.

    Relying on your divides implementation, define a function,
    which checks whether a given non-negative integer is a prime number.
-}

-- Recursive solution
divides :: Integer -> [Integer]
-- Starting from 2 and adding 1 to divisors, as every number is divisible by 1
divides n = divides' (abs n) 2 [1]
  where
    divides' n divisor divisors
      | n == divisor = divisors ++ [n]
      | mod n divisor == 0 = divides' n (divisor + 1) (divisors ++ [divisor])
      | otherwise = divides' n (divisor + 1) divisors

-- List comprehension solution
divides2 :: Integer -> [Integer]
divides2 n = [x | x <- [1..(div n' 2)], mod n' x == 0] ++ [n']
  where n' = (abs n)

isPrime :: Integer -> Bool
isPrime n
  | n < 0 = error "negative number provided!" -- False was also applicable
  | otherwise = divides2 n == [1, n]

{-
  Exercise 3

  Write a function prefix :: String -> String -> Bool,

  which for any two given strings checks whether the first one is a prefix
  of the other one.

  Relying on your prefix implementation, define a function substring,
  checking whether one given string is a part of another one.
-}

prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False -- To make this function argument order-agnostic, change this to True
prefix (x:xs) (y:ys) = x == y && prefix xs ys

substring :: String -> String -> Bool
substring _ [] = False -- To make this function argument order-agnostic, remove this
substring st1 st2@(_:xs) = prefix st1 st2 || substring st1 xs

{-
  Exercise 4

  Write a function permut :: [Integer] -> [Integer] -> Bool,
  which checks that the two given lists of integers are permutations of each other.
  A permutation means that the lists consist of the same elements,
  occuring the same number of times.
-}

permut :: [Integer] -> [Integer] -> Bool
permut xs ys = sort xs == sort ys

-- There were no restrictions on whether using additional libraries were allowed,
-- if not, then I would reuse quickSort algorithm from Lecture 5 and would still
-- sort it first and then compare it.

{-
  quickSort :: [Integer] -> [Integer]
  quickSort [] = []
  quickSort  (x:xs) = quickSort less_eq_than_x ++ [x] ++ quickSort greater_than_x
    where
      less_eq_than_x = [y | y <- xs, y <= x]
      greater_than_x = [y | y <- xs, y > x]
-}

{-
  Exercise 5

  Using the list comprehension method, define a function

  capitalise :: String -> String,

  which modifies a given string by filtering (leaving only letters) and then
  changing the found letters into capital ones.
-}

capitalise :: String -> String
capitalise str = [toUpper chr | chr <- str, elem chr letters]
  where
    letters = ['a'..'z'] ++ ['A'..'Z']

{-
  Again, if external libraries are not allowed,
  then my toUpper would look like this,
  referenced from given solution in
  [https://stackoverflow.com/questions/20478982/first-attempt
  -at-haskell-converting-lower-case-letters-to-upper-case/20479476]
-}

toUpper' :: Char -> Char
toUpper' chr
  | chr < 'a' || chr > 'z' = chr
  | otherwise = toEnum (fromEnum chr - offset)
  where
    offset = 32


{-
  Exercise 6

  A shop basket can be defined as the data structure [(String,Float)],
  storing a pairs of items and their prices. Write two functions:

  itemTotal :: [(String,Float)] -> [(String,Float)],
  which merges repeating items, summing their prices accordingly.

  itemDiscount :: String -> Integer -> [(String,Float)] -> [(String,Float)],
  which applies a discount (the second parameter, ranging from 0% until 100%)
  to a given item (the first parameter) and correspondingly modifies
  the prices of this item in a given shop basket (the third parameter).
-}

itemTotal :: [(String, Float)] -> [(String, Float)]
itemTotal [] = []
itemTotal basket@((item,_):_) = (item, summed_price) : itemTotal remaining_items
  where
    summed_price = sum [price | (item',price) <- basket, item' == item]
    remaining_items = [y | y@(item',_) <- basket, item' /= item]

itemDiscount :: String -> Integer -> [(String, Float)] -> [(String, Float)]
itemDiscount item discount basket
  | numBetween discount 0 100 = map applyDiscount basket
  | otherwise = error "discount must be between 0 and 100!"
  where
    applyDiscount (a, b)
      | a == item = (a, b - (b * (fromIntegral discount) / 100.0))
      | otherwise = (a, b)

numBetween :: Integer -> Integer -> Integer -> Bool
numBetween a lowerBound upperBound
  | a >= lowerBound = a <= upperBound
  | otherwise = False
