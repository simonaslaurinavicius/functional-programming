module Lecture5 where

import Data.Char

-- List comprehensions

-- In a list comprehension, we define a list in terms
-- of the elements of another list


-- General syntax:
-- [res_expression (expresion on)
-- | source_element (some elements)
-- <- source_list (from existing list), guards (where elements satisfy these)]

-- Another (quite powerful) list constructor

-- Inspired by the notion of mathematical set comprehension

input_list = [2, 4, 15]
list1 = [2 * n | n <- input_list] -- [4, 8, 30]

-- No need to preserve the type of existing list elements
list2 = [isEven n | n <- input_list] -- [True, True, False]

isEven n = (mod n 2 == 0)

-- With guards
list3 = [n * n | n <- input_list, isEven n, n > 3] -- [16]

input_list2 = [(2, 3), (2, 1), (7, 8)]
list4 = [m + n | (m, n) <- input_list2 ] -- [5, 3, 15]

-- List comprehension can be used to define functions, for example
-- filtering the list out

allEven xs = (xs == [x | x <- xs, isEven x])
allOdd xs = ([] == [x | x <- xs, isEven x])

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

acronym :: String -> String
acronym st = [ch | ch <- st, elem ch ['A'..'Z']]

-- Important: list comprehension expression can have more than one source list!

pairs = [(x, y) | x <- [1, 2, 3], y <- "ab"]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
-- It's clear that the order is take first element from x list
-- and go through all ements from y list, then take the second
-- element from the x list and so on.

-- One of the main advantages of this order is that we can use
-- the element from the first list as a variable in the second list

triangle :: Int -> [(Int, Int)]
triangle n = [(x, y) | x <- [1..n], y <- [1..x]]

-- pattern <- listExpression is called a generator
-- booleanExpression is called a test (guard statement)

-- Scalar product of two vectors
multVec :: [Float] -> [Float] -> Float
multVec xs ys = sum [x * y | (x, y) <- zip xs ys]

-- Pythagorean Triples -  x*x + y*y == z*z
pyTriples :: Integer -> [(Integer, Integer, Integer)]
pyTriples n = [(x, y, z) | x <- [2..n], y <- [x+1..n], z <- [y+1..n], x * x + y * y == z * z]

-- Primitive recursion on lists

doubleAll :: [Integer] -> [Integer]
doubleAll [] = []
doubleAll (x:xs) = 2 * x : doubleAll xs

selectEven [] = []
selectEven (x:xs)
  | isEven x = x : selectEven xs
  | otherwise = selectEven xs

-- Some kind of insertion sort
insertionSort :: [Integer] -> [Integer]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert x [] = [x]
    insert x (y:ys)
      | x <= y = x : (y:ys)
      | otherwise = y : insert x ys

-- Our implementation of standard filter function
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter predicate (x:xs)
  | predicate x = x : myFilter predicate xs
  | otherwise = myFilter predicate xs

-- Using list comprehension to implement filter function
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 predicate xs = [x | x <- xs, predicate x]

-- adjacent - function that filters out elements from the list that
-- have *both* their adjacent elements equal

adjacent :: Eq a => [a] -> [a]
-- adjacent (x:y:z:zs)
--   | x == z = y : adjacent (y:z:zs)
--   | otherwise = adjacent (y:z:zs)
-- adjacent _ = []

-- Simplifying the solution using @(as) pattern
adjacent (x:l@(y:z:zs))
  | x == z = y : adjacent l
  | otherwise = adjacent l
adjacent _ = []

-- Helper functions with accumulators
sum_not_exceeding :: Int -> [Int] -> [Int]
sum_not_exceeding n xs = not_exceed' n xs 0
  where
    not_exceed' _ [] _ = []
    not_exceed' n (x:xs) acc
      | (acc + x) > n = []
      | otherwise = x : (not_exceed' n xs (acc + x))
