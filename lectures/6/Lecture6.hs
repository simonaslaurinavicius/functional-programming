module Lecture6 where

-- Tail recursion - a way to improve performance of recursion

len :: [a] -> Integer
len [] = 0
len (x:xs) = 1 + len xs -- (1 + (1 + (1 + ...)))

-- For a large input list, this function creates large (!) call stacks,
-- which can lead to a drop in performance and/or stack overflow.

-- One way to improve on this is to rewrite this using tail recursion.
-- Making this code - tail recursive.

tr_len xs = len' xs 0
  where
    len' [] n = n
    len' (_:xs) n = len' xs (n + 1)  -- len' xs 1, then len' xs 2, and so on...

-- Intermediate result is calculated and passed as an extra parameter (n + 1),
-- we don't wait inside one function for other function to return but
-- increment the parameter and move on.

-- Tail recursion usually means that recursive code can be optimised into
-- a traditional loop (tail call optimisation)


-- Generic functions (polymorphism)
-- A function is polymorphic if it can be applied for aguments of many
-- different types. This is achieved by using type variables in function
-- type signature.

-- For example, take function length, it's type signature looks like this:
-- length:: [a] -> Int

-- It takes a list and we don't care what type list elements are, hence we
-- use type variable - a.

-- Polymorphism and overloading - two mechanisms by which the same function
-- name can be used with different types

-- An overloaded function: different function definitions for different types
-- but with the same functio name

-- Example: the overloaded operator for equality comparison (==) can
-- have very different definitions for different types

-- (==) :: Eq a => Eq b => (a, b) -> (a, b) -> Bool
-- (==) (x1, y1) (x2, y2) = (x1 Prelude.== x2) && (y1 Prelude.== y2)
-- Equality operator definition used in the overloaded definition
-- can be completely different from the overloaded (==) operator we are
-- defining.

-- Goal: split a string into a list of words (smaller strings). Whitespaces
-- and punctuation should not be taken into account.

whitespaces :: [Char]
whitespaces = ['\n', '\t', ' ']

punctuation :: [Char]
punctuation = ['.', ',', ';', '-', ':']

delimiters :: [Char]
delimiters = whitespaces ++ punctuation

-- Returning the first word
getFirstWord :: String -> String
getFirstWord [] = []
getFirstWord (x:xs)
  | elem x delimiters = []
  | otherwise = x : getFirstWord xs

-- Return string without the first word
dropFirstWord :: String -> String
dropFirstWord [] = []
dropFirstWord l@(x:xs)
  | elem x delimiters = l
  | otherwise = dropFirstWord xs

-- Both functions work incorrectly for leading spaces, we need to remove them

dropLeadingSpaces :: String -> String
dropLeadingSpaces [] = []
dropLeadingSpaces l@(x:xs)
  | elem x delimiters = dropLeadingSpaces xs
  | otherwise = l


splitWords :: String -> [String]
splitWords [] = []
splitWords st
  | filtered_st == "" = []
  | otherwise = (getFirstWord filtered_st) : splitWords (dropFirstWord filtered_st)
  where
    filtered_st = dropLeadingSpaces st

-- Can we simplify this function? Have a function that returns both the first word
-- and the remainder of the string, after removing the leading spaces first?

splitFirstWord :: String -> (String, String)
splitFirstWord st = (first_word, rem_st)
  where
    filtered_st = dropLeadingSpaces st
    first_word = getFirstWord filtered_st
    rem_st = drop (length first_word) filtered_st

splitWords2 :: String -> [String]
splitWords2 [] = []
splitWords2 st = first : splitWords2 rest
  where
    (first, rest) = splitFirstWord st


-- General recursion on lists

-- Any recursive call to the value on a simpler (smaller) list will
-- be legitimate and will lead to the function termination.

-- The main difference from primitive recursion is that in primitive
-- recursion we have a recursive call on the list tail, whereas in general
-- case the tail can be a simpler (smaller) list and has no size constraints.

-- The general question to ask - in defining f xs (where xs is non-empty),
-- which values of ys that is a sublist of xs would help us to work out the
-- answer?

occurrences :: [Integer] -> [(Integer, Int)]
occurrences [] = []
occurrences (x:xs) = (x, length x_occurences + 1) : (occurrences non_x_occurences)
  where
    x_occurences = [x' | x' <- xs, x' == x]
    non_x_occurences = [x' | x' <- xs, x' /= x] -- Filter out x's and continue


quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort  (x:xs) = quickSort less_eq_than_x ++ [x] ++ quickSort greater_than_x
  where
    less_eq_than_x = [y | y <- xs, y <= x]
    greater_than_x = [y | y <- xs, y > x]

-- Let expressions


-- A variation of local definitions

-- Contrary to where definitions, let expressions can be used
-- within almost any Haskell expression

-- Let Expressions.

-- Haskell's let expressions are useful whenever a nested set
-- of bindings is required. As a simple example, consider:

-- let y   = a*b
--     f x = (x+y)/y
-- in f c + f d

-- The set of bindings created by a let expression is mutually recursive,
-- and pattern bindings are treated as lazy patterns
-- (i.e. they carry an implicit ~).
-- The only kind of declarations permitted are type signatures,
-- function bindings, and pattern bindings.

-- Where Clauses.

-- Sometimes it is convenient to scope bindings over several guarded equations,
-- which requires a where clause:

-- f x y  |  y>z           =  ...
--        |  y==z          =  ...
--        |  y<z           =  ...
--      where z = x*x

-- Note that this cannot be done with a let expression,
-- which only scopes over the expression which it encloses.

-- A where clause is only allowed at the top level of a set of equations
-- or case expression. The same properties and constraints on bindings
-- in let expressions apply to those in where clauses.
-- These two forms of nested scope seem very similar,
-- but remember that a let expression is an expression,
-- whereas a where clause is not, it is part of the syntax
-- of function declarations and case expressions.

calculateBMI :: [(Float, Float)] -> [Float]
calculateBMI xs = [bmi | (w, h) <- xs, let bmi = w / h^2]

-- Case expressions

-- The case construction allows us to define a result by pattern matching
-- over an arbitrary Haskell expression

-- The general form of the expression

-- case e of
  -- p1 -> e1
  -- p2 -> e2
  -- ...
  -- pn -> en

-- Order matters! The first pattern pi matching will give us the result - ei

firstDigit :: String -> Char
firstDigit st =
  case (digits st) of
    [] -> error "there are no digits in the string!"
    (x:xs) -> x
  where
    digits :: String -> String
    digits st = [ch | ch <- st, elem ch ['0'..'9']]
