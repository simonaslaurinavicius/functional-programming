module Lecture3 where

import Test.QuickCheck

fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | n > 0 = fact (n - 1) * n
  | otherwise = error "Negative argument!"

fact2 :: Integer -> Integer
fact2 0 = 1
fact2 n
  | n > 0 = fact2 (n - 1) * n
  | otherwise = error "Negative argument!"

-- Sum of factorials
sumFacts :: Integer -> Integer
sumFacts 0 = 1
sumFacts n
  | n > 0 = sumFacts (n - 1) + fact n
  | otherwise = error "Negative argument!"

-- Generalization of above - sum of functions applied to range (0..n)
sumFuns :: (Integer -> Integer) -> Integer -> Integer
sumFuns f 0 = f 0
sumFuns f n
  | n > 0 = sumFuns f (n - 1) + f n
  | otherwise = error "Negative argument!"

-- Redefining sumFacts as a special case of sumFuns
sumFacts2 n = sumFuns fact n

prop_sumFacts :: Integer -> Bool
prop_sumFacts n =
  if n >= 0 then sumFacts2 n == sumFacts n else True

-- Generalizing even further - sum of functions applied to range (m..n)
sumFuns2 :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumFuns2 f m n
  | m == n = f m
  | m < n = sumFuns2 f (m + 1) n + f m
  | otherwise = error "Invalid argument!"

-- Extending recursion (more general forms)

-- Multiple base cases, multiple recursive calls
fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | n > 1 = fib (n - 2) + fib (n - 1)

-- Integer division
divide :: Integer -> Integer -> Integer
divide m n
  | (m < 0) || (n <= 0) = error "Negative argument!"
  | m < n = 0 -- Main thing - the number of base cases is theoretically infinite
  | otherwise = divide (m - n) n + 1


-- Example of mutually dependent definitions
-- When compiling, these functions are merged into a single
-- function.
isOdd, isEven :: Int -> Bool
isOdd n
  | n <= 0 = False
  | otherwise = isEven (n - 1)

isEven n
  | n < 0 = False
  | n == 0 = True
  | otherwise = isOdd (n - 1)

applyPositiveIfTrue :: (Integer -> Integer, Integer, Bool) -> Integer
applyPositiveIfTrue (_, x, False) = x
applyPositiveIfTrue (f, x, _)
  | x >= 0 = f x
  | otherwise = f (-x)

-- Tuples inside tuples
shift :: ((a, b), c) -> (a, (b, c))
shift ((x, y), z) = (x, (y, z))
