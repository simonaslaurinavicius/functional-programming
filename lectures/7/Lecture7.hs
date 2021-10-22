module Lecture7 where

{-
  Enumerated types are the simplest examples of user-defined datatypes, they
  are defined by simply listing all its members one by one:

  `data` keyword is used to construct them.
-}

-- data Bool = False | True
--   deriving (Show, Eq, Ord)

data Temp = Cold | Lukewarm | Hot
  deriving (Show, Eq, Ord)

-- `deriving` allows to inherit some type features like the ability to be printed,
-- compared to be equal, ordered by the way they are defined (from smaller to greater).

-- For example `Hot > Cold` returns True, but `Hot < Cold` returns False, this is because
-- Cold comes before Hot and is "smaller"

-- Example: rock, paper, scissors
data Move = Rock | Paper | Scissors
  deriving (Show, Eq)

-- Define the rules
beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat Scissors = Rock

-- It's really neat that the datatype elements can be directly used as literals
-- for simple pattern matching


{-
  Product types - a datatype with a number of components or fields

  A constructor function is used to combine these fields together into one object.
-}

data People = Person Name Age
  deriving (Show, Eq)

type Name = String
type Age = Int

-- type introduces a synonym for a type and uses the same data constructors.

{-
  To construct an object of the type People, we use the constructor Person with two
  values of the type String and Int
-}

-- Example: geometrical shapes

data Shape = Circle Float | Rectangle Float Float
  deriving (Show, Ord, Eq)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

isRound :: Shape -> Bool
isRound (Circle _) = True -- Circle with any radius
isRound (Rectangle _ _) = False -- Any rectangle

-- Both constructors and their values are used for pattern matching

-- Product types vs tuples

-- type Person = (Name, Age) - Person as a Tuple

-- Advantages of product datatype:

-- Each object carries an explicit label (constructor name) of the element purpose
-- If is not possible to treat an arbitrary pair of (String, Int) as a person.
-- The datatype will appear in all typing error messages (in case of a tuple, the type
-- name will be expanded)

-- Advantages of tuple type:

-- The elements are more compact and so definitions are shorter
-- Many polymorphic functions over pairs and tuples in general can be reused

-- Two things - element names in enumerated types - nullary constructors
-- (constructors with no args)

-- The datatypes can be recursive or polymorphic

-- With user-defined datatypes we can use objects of different types even when a single
-- type is required (like in lists)

-- Different types are "separated" or distinguished by different constructors, while on
-- the outside the elements are of the same datatype

data MyDatatype = Name String | Number Float
  deriving (Show, Eq)

my_list :: [MyDatatype]
my_list = [Name "Simonas", Number 42.16]


-- Generalising computations in Haskell

-- 1. Defining polymorphic functions to work for all or many different concrete types

-- 2. Implement reusable patterns of computations using higher-order functions, i.e.,
-- functions that take other functions (implementing particular kind of computations)
-- as parameters

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

-- Combining zip with map
zipWithMap :: ((a, b) -> c) -> [a] -> [b] -> [c]
zipWithMap f xs ys = map f (zip xs ys)

-- Not working for (+)

-- Property (e.g., of being a digit, an even number, etc.) is expressed
-- using a function of type t -> Bool, where t is some concrete type we are
-- dealing with.

-- Filter is a function which accepts a property and a list and filters it
-- by checking whether the elements of the list satisfy the property

-- filter p xs = [x | x <- xs, p x]

-- digits :: String -> String
-- digits xs = filter isDigit xs

-- Skipping or continuing until/while some property holds
getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p [] = []
getUntil p (x:xs)
  | p x = []
  | otherwise = x : getUntil p xs

-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile p [] = []
-- takeWhile p (x:xs)
--   | p x = x : takeWhile p xs
--   | otherwise = []

-- takeWhile (dropWhile) - predefined in Haskell (either Prelude or Data.List)

-- Folding elements according to the given function

-- Folding (by parentheses) happens on the right first
myFoldr :: (a -> a -> a) -> a -> [a] -> a
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- The first argument: a binary function over an arbitrary type a
-- The second argument: the starting value of the type a (used for empty list case)
-- The third argument: list to be folded

myFoldl :: (a -> a -> a) -> a -> [a] -> a
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs
