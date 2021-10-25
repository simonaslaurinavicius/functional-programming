module Lecture8 where

-- Redefining list reversion via folding
rev :: [a] -> [a]
rev xs = foldr snoc [] xs

-- Opposite of ':' operator - adds an element to the *end*
-- of the list

-- snoc comes from the ':' being called cons, so snoc - reverse of cons
-- Folding functions accept accumulator and a single element
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

-- Functional composition - simple way of structuring functional program by composing
-- together separate functions

-- It wires two functions together by passing the output of one to the input
-- of the other. As a result, new composite function is automatically created.

-- It is defined as the operator - (.)
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (f . g) x = f (g x)

-- Execution steps: g :: (a -> b), then pass it's output to (f :: b -> c) and execute f
-- thus the result - (f . g) :: (a -> c)

-- The (.) might be confusing - the execution order is oposite to the order of
-- function appearance.

-- Another version (>.>) - forward function composition
-- Order is explicit from the operator
-- f >.> g = g . f

-- Examples of function composition:

-- Negate everything!
-- (negate . abs) 5 => -5

-- Sum each list tails and negate the sum
-- map (negate . sum . tail) [[1..5], [3..6], [1..7]] => [-14, -15, -27]

makeNegatives :: [Integer] -> [Integer]
makeNegatives = map (negate . abs)

someFunction :: Integer -> Integer
someFunction = sum . (replicate 8) . negate

-- Function application

-- Typical use - f x

-- But there is also explicit operator of function application:
-- ($) :: (a -> b) -> a -> b
-- f $ e = f e

-- Why is it needed?
-- Many Haskell programmer use $ as an alternative to parentheses
-- Sometimes we need the application operator as a function that, e.g. we
-- pass as an argument
-- zipWith ($) [sum, product] [[1, 2], [3, 4]] => sum [1,2], product [3, 4]

-- Remember: function application is left associative so that f x y means
-- (f x) y, not f (x y)

-- Function type symbol -> is right associative so that a -> b -> c
-- means a -> (b -> c), not (a -> b) -> c

-- Lambda abstractions
-- All functions are stored as lambda abstractions in Haskell memory

-- Notation:
-- \x -> x + 1 == addOne x = x + 1

-- Lambda abstraction passed as a parameter, where function is expected
-- map (\x -> x + 1) [2, 3, 4]
-- or function result
-- addNum n = (\m -> n + m)

-- Equivalent ways to define a function
mThree x y z = x * y * z
mThree' x y = \z -> x * y * z
mThree'' x = \y z -> x * y * z
mThree''' = \x y z -> x * y * z -- This is the way all the functions above
-- will be stored in Haskell memory

-- \x y -> z => shorthand for \x -> (\y -> z)

mapFuns :: [a -> b] -> a -> [b]
mapFuns [] x = []
mapFuns (f:fs) x = f x : mapFuns fs x

mapFuns' :: [a -> b] -> a -> [b]
mapFuns' fs x = map (\f -> f x) fs

-- Complex function applications with lambda abstractions
-- complexComposition f g = (\x y -> g (f x) (f y))

-- Partial function application

-- multiply x y = x * y => multiply = (\x y -> x * y) => multiply (\x -> (\y -> x * y))
-- Thus, each Haskell function takes **one parameter at a time**!

-- multiply 3 = (\y -> 3 * y) => beautiful! Very convenient technique to adapt a function
-- to our needs!

-- We can now define a function for doubling all list elements simply as:
-- doubleAll :: [Integer] -> [Integer]
-- doubleAll = map (multiply 2)

-- This solves the problem when map requires a single argument
-- transformation function (of general type a -> a) as its first parameter.

-- Partial function application adjusts the function and makes it applicable.

-- Partial applied operators - amazing!
-- Depending on the side we put an argument, the argument is automatically
-- partially applied to either the first or second argument.

-- Examples:
-- (+2) - a function which adds to its argument.
-- (2+) - a function which adds to its argument.
-- (>2) - a function which checks whether a given number is greater than 2.
-- (2>) - a function which checks whether a given number is smaller than 2.
-- (3:) - a function which adds 3 to the list beginning.
-- (++ "!!!") - a function which adds exclamations to the end of the string.
-- ($ 3) - a function which applies a given function to integer 3

-- Partial applied operators examples

-- Return a function, which takes a list, increases the values by 100,
-- then filters out the values which are less than 0
ff = filter (>0) . map (+100)

doubleAll = map (*2)

-- Defining elem as an operator to allow to create a partial application
-- with either the first or the second argument filled out
(%%) :: Eq a => a -> [a] -> Bool
(%%) = elem

-- is_whitespace = (%% whitespace)

-- What if we want to partially apply a function, but not to its first argument?

-- Redefine elem by switching its arguments:

member xs x = elem x xs
is_whitespace = member whitespace

-- Apply some argument switching higher-order function (like flip from Prelude)
is_whitespace = (flip elem) whitespace

-- Use lambda abstraction:
is_whitespace = (\ch -> elem ch whitespace)

-- Curried function (from Haskell Brook Curry name)

-- Curried form :: T1 -> T2 -> ... -> TN
-- General form :: (T1 -> T2 -> ...) -> TN
curry :: ((a, b) -> c) -> a -> b -> c
curry g x y = g (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- Example with `zip` and `unzip`
-- zip (unzip) [(1, True), (99, False)]

-- (uncurry zip) (unzip [(1, True), (99, False)])

-- Summary: various ways to define a function - negate all elements in a list
-- and multiply them together
prodNegated :: [Integer] -> [Integer]
prodNegated xs = foldr (*) (map negate xs)

prodNegated' xs = (foldr (*) 1 . map negate) xs

prodNegated'' xs = foldr (*) 1 $ map negate $ xs

prodNegated''' xs = \xs -> (foldr (*) 1 . map negate) xs

prodNegated'''' = foldr (*) 1 . map negate

prodNegated''''' = foldr (\x y -> negate x * y) 1 -- Dropping map, by calling negate on
-- each element of the list explicitly
