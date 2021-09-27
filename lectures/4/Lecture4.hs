-- Simple aggregate/collection types

import Test.QuickCheck

-- Creating a tuple using (...) constructor
minAndMax :: Integer -> Integer -> (Integer, Integer)
minAndMax x y
  | x >= y = (y, x)
  | otherwise = (x, y)

-- Pattern matching a tuple as an argument
addPair :: (Integer, Integer) -> Integer
addPair (x, y) = x + y

-- Pattern matching with literals
multPair :: (Integer, Integer) -> Integer
multPair (0, _) = 0
multPair (_, 0) = 0
multPair (x, y) = x * y

-- Here _ is a special symbol called wildcard used
-- to match any value.

-- Polymorphic function with tuples inside tuples
shift :: ((a, b), c) -> (a, (b, c))
shift ((x, y), z) = (x, (y, z))

-- Remember: Integer is an arbitrary precision type. It will hold
-- a number as big as your machine memory allows. Int is the traditional
-- integral type, which can be either 32 or 64 bits, depends on
-- the machine architecture (though not less than 30 bits).
weird :: (Int -> Int -> Int, (Int, Int), Bool) -> Int
weird (_, _, False) = (-1)
weird (_, (0, _), _) = 0
weird (_, (_, 0), _) = 0
weird (f, (x, y), _) = f x y


-- Lists - a collection of items comprised of a same type t

x :: [Integer]
x = [1, 2, 3, 4]

y :: [Integer -> Integer -> Integer]
y = [min, max]

-- Basic operations
-- : -> list constructor, adds an element to the beginning of a list
-- head -> extract first element of non empty list
-- tail -> returns the list with first element removed
-- length -> list length
-- ++ -> concanate (merge) two lists
-- null -> check if list is empty

-- Range constructor, follows a pattern specified by the first element
-- last element is treated as a bound after which to stop
range :: [Integer]
range = [2 .. 7]

range2 :: [Float]
range2 = [2.1 .. 7]

range3 :: [Char]
range3 = ['a' .. 'z']

-- We can also use ranges with the given step
range4 :: [Integer]
range4 = [13, 11 .. 8] -- > [13, 11, 9], the next element would be 7, but the bound is 8

-- Strings -> special case of lists (character lists)
equal :: Bool
equal = "valio!" == ['v', 'a', 'l', 'i', 'o', '!']

-- Every list is either empty ([]) or non-empty

-- If a list is non-empty, it can be written in the form x:xs,
-- where x is the head and xs is the tail of the list, for example:
-- [4, 2, 12] == 4:[2, 12]

-- Lists are stored in the Haskell memory like this:
-- [4,2,12] == 4:(2:(12:[]))
-- Using ':' constructor and empty list

-- Thus we have two standard patterns for constructing lists
-- x:xs and [...] - primary constructors

-- Pattern matching on lists

myHead :: [a] -> a
myHead [] = error "empty list!"
myHead (x:_) = x

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

describeList :: [a] -> String -- equivalent to [Char]
describeList xs = "The list is " ++  what xs
  where
    what [] = "empty."
    what [_] = "a singleton."
    what _ = "comprised of multiple elements."
    -- _ and (_:_:_), and (_(_:_)) are all syntactically equivalent

-- Literals can be considered as (nullary) constructors - constructors
-- without parameters


-- Important (!) Pattern matching vs guards

test :: Integer -> Integer
test 1 = 2 -- Pattern matching
test x -- Guards [x is actually not part of guards - guards don't bind anything]
  | x == 2 = 3
  | otherwise = 0

-- Matching literal values: can be done by both

-- More complex value comparisons (not just equality/inequality): only by guards

-- Matching agains the argument structure (e.g, a tuple of three elements,
-- singleton list): only by pattern matching

-- The good news: we can combine both of them

-- From StackOverflow
-- [https://stackoverflow.com/questions/4156727/
-- what-is-the-difference-between-pattern-matching-and-guards]

{-
  Guards are both simpler and more flexible:
  They're essentially just special syntax that translates to a series of
  if/then expressions. You can put arbitrary boolean expressions in the guards,
  but they don't do anything you couldn't do with a regular if.
-}

{-
  First pattern matching can not evaluate arbitrary conditions.
  It can only check whether a value was created using a given constructor.

  Second pattern matching can bind variables.
  So while the pattern [] might be equivalent to the guard null lst
  (not using length because that'd not be equivalent - more on that later),
  the pattern x:xs most certainly is not equivalent to the guard not (null lst)
  because the pattern binds the variables x and xs, which the guard does not.

  A note on using length: Using length to check whether a
  list is empty is very bad practice, because, to calculate
  the length it needs to go through the whole list, which will take O(n) time,
  while just checking whether the list is empty takes O(1) time with null or pattern matching.
  Further using `length´ just plain does not work on infinite lists.
-}

ff :: [Int] -> Int
ff [] = 0
ff [x]
  | x == 0 = 100
  | otherwise = x
ff (x:y:xs) = x + y + ff xs

-- Same function using only patterns
ff2 :: [Int] -> Int
ff2 [] = 0
ff2 [0] = 100
ff2 [x] = x
ff2 (x:y:xs) = x + y + ff2 xs

propFF :: [Int] -> Bool
propFF x =
  ff x == ff2 x

-- Function that sums integers in tuple only if the second tuple element is True
condSum :: [(Integer, Bool)] -> Integer
condSum [] = 0
condSum ((x, True):xs) = x + condSum xs
condSum ((_, False):xs) = condSum xs

-- Pattern matching when binding a global or local identifier
(x', y') = (10, (True, "abc")) -- x' = 10, y' = (True, "abc")

buildTriple :: a -> b -> c -> (a,b,c)
buildTriple x y z = (x, y, z)
(_,_,w) = buildTriple True "Hurray!" 999 -- w = 999

(z:rest) = [1, 2, 3] -- z = 1, rest = [2, 3]
(first:_) = [4, 6 .. 24] -- first = 4
(_:second:_) = [4, 6 .. 24] -- second = 6
(e1:e2:other) = [10 .. 20] -- e1 = 10, e2 = 11, other = [12 .. 20]

-- as(@)-pattern
-- Allows us to retain the initial structure
-- What it means?
-- I can both use s1, s2, s3  as pattern matched bindings (1, 3, [5..13])
-- and also use the identifier to reference the whole list ([1, 3 .. 13])
identifier@(s1:s2:s3) = [1, 3 .. 13]

-- General primitive recursive pattern with lists
fun :: [t] -> Int
fun [] = 1 -- print "base case!"
fun (x:xs) = fun xs -- print "recursive case!"

myElem x [] = False
myElem x (y:ys) = (x == y) || myElem x ys
-- Haskell uses short-circuit evaluation meaning that if (x == y) returns True
-- the recursive call to myElem is never performed as the result will be true
-- nevertheless

remove _ [] = []
remove x (y:ys)
  | x == y = remove x ys
  | otherwise = y:(remove x ys)
