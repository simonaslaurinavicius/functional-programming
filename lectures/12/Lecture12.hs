module Lecture12 where

import Data.List ((\\))
-- Lazy evaluation in Haskell

-- Haskell will only evaluate an argument to a function if that
-- argument's value is needed to compute the overall result.

-- One of the consequences: a possibility to describe *infine
-- data structures*. Under lazy evaluation, often only parts of such
-- a data structure need to be examined.

-- Technical details:

-- When the Haskell evaluation process starts, a thunk is created for
-- each expression.

-- A thunk - placeholder in the underlying graph of the program. It
-- will be evaluated, if necessary. Otherwise, the GC will eventually
-- sweep it away.

-- If it is evaluated, because it's in the graph, it can be shared
-- between expressions without re-calculation.

-- Lazy evaluation is often compared to non-strictness

-- Strict vs non-strict languages

-- Strict languages evaluate inside out; nonstrict languages like Haskell
-- evaluate outside in.

-- Outside in means that evaluation proceeds from the outermost
-- parts of expressions and works inward based on what values are needed.

-- Thus, the order of evaluation and what gets evaluated can vary
-- depending on inputs.

-- Example, which works only in a nonstrict language (Haskell, in this case):

-- fst (1, undefined) -- undefined - an exceptional value that can be an element of any type.
-- tail [undefined, 2, 3]

-- We don't evaluate (1, undefined) or [undefined, 2, 3] first, as
-- if we did, we would get an error.

-- Evaluation is from outside in. In situation like

-- f1 e1 (f2 e2 17)

-- f1 with arguments is evaluated first and only if needed
-- f2 with arguments is evaluated.

-- In other cases we are evaluating traditionally - from left to right


-- Evaluation order in list comprehension

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = [(x, y) | x <- xs, y <- ys]

-- [*Lecture12] λ > pairs [1,2,3] [4,5]

-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]

-- Taking first element from xs, iterating with it through second list,
-- taking second element from xs, ...

-- Usage: Pythagorean Triples

pyTriples :: Integer -> [(Integer, Integer, Integer)]
pyTriples n = [(x, y, z) | x <- [2..n], y <- [x+1..n], z <- [y+1..n],
  x^2 + y^2 == z^2]


perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ps | x <- xs, ps <- perms(xs\\[x])]

-- Infinite data structures

-- Lists

ones :: [Integer]
ones = 1 : ones

-- take 5 ones => [1,1,1,1,1]

odds :: [Integer]
odds = [3, 5 ..]

-- Simple implementation of infinite list starting from some number n
from :: Integer -> [Integer]
from n = n : from (n+1)

-- Infinite Pythagorean triples
infinitePyTriples :: [(Integer, Integer, Integer)]
infinitePyTriples = [(x,y,z) | z <- [2..], y <- [2..z-1], x <- [2..y-1],
  x^2 + y^2 == z^2]

-- Generating primes using Sieve of Eratosthenes
primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, (mod y x) > 0]

-- Generating pseudo-random numbers
nextRand ::  Integer -> Integer
nextRand n = (multiplier*n + increment) `mod` modulus

randomSequence ::  Integer -> [Integer]
randomSequence = iterate nextRand

seed = 17489
multiplier = 25173
increment = 13849
modulus = 65536

-- Abstract patterns and algebras

-- Algebra => one or more operations and the set the operate over

-- A word frequently used to describe abstract patterns in code

-- Examples of such algebras: monoids, semigroups, functors, monads...

-- They are implemented using type classes

-- Type classes define the set of operations, while their instances
-- define how each operation will perform for a given type or set


-- In mathematics, a monoid is an algebraic structure
-- with a single associative binary operation and an identity element

-- In other words, it is a data type for which we can define a binary function such as:
--  - the function takes two parameters of the same type;
--  - there exists such a value that does not change other values
--    when used with the function (identity element);
--  - If we have three or more values and use the function to reduce
--    them to a single result, the application order does not matter (associativity).


-- Examples: Integer with (*) and 1, List a ([a]) with (++) and []


-- Monoid definition in Haskell

-- class Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mconcat :: [a] -> a
--   mconcat = foldr mappend mempty


-- mempty - identity element
-- mappend - binary monoid operation
-- mconcat - generalisation of mappend over a list of values


-- Lists are monoids:

-- instance Monoid [a] where
--  mempty = []
--  mappend = (++)

-- Maybe a is a monoid:

-- instance Monoid a => Monoid (Maybe a) where
--  mempty = Nothing
--  Nothing `mappend` m = m
--  m `mappend` Nothing = m
--  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- Three mathematical properties (laws) that are expected from
-- any monoid instance.

-- Left identity
-- mappend mempty x = x

-- Right identity
-- mappend x mempty = x

-- Associativity
-- mappend x (mappend y z) = mappend (mappend x y) z

-- Functor – pattern of mapping over or around some structure
--  that we do not want to alter

-- That is, we want to apply the function to the value
-- that is ”inside” of some structure and leave the structure intact

-- Example: a function gets applied for each element
--  of a list and the list structure remains.

-- No elements are removed or added, only transformed
-- The type class Functor generalises this pattern for many types of structure

-- Definition

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Prelude> fmap (*10) [2,7]
-- [20,70]
-- Prelude> fmap (+1) (Just 1)
-- (Just 2)
-- Prelude> fmap (+1) Nothing
-- Nothing
-- Prelude> fmap (+10) (4,5)
-- (4,15)
-- Prelude> fmap (++ "Esq.") (Right "Chris Allen")
-- (Right "Chris Allen, Esq.")

-- What is f inside Functor class?

-- Type constructors: functions that take types and produce types.
-- Examples: []; (,); Maybe; Either; Tree ...
-- User-defined data type names are also type constructors,
-- if the type definition contains at least one type variable

-- The Functor type class is parameterised over
-- such a type constructor (f)

-- Essentially, f introduces the structure that fmap works inside on!

-- Example of list type constructor
-- [] Int <=> [Int]

-- Thus map is a special case, where f type constructor is equal to []

-- In other words, lists are an instance of the Functor type class:

-- instance Functor [] where
--  fmap = map

-- mapTree ::  (a-> b) -> Tree a -> Tree b
-- mapTree Nil = Nil
-- mapTree f (Node x t1 t2) =
--   Node (f x) (mapTree f t1) (mapTree f t2)

-- instance Functor Tree where
--   fmap = mapTree

-- mapMaybe :: (a->b) -> Maybe a -> Maybe b
-- mapMaybe g Nothing  = Nothing
-- mapMaybe g (Just x) = Just (g x)

-- instance Functor Maybe where
--   fmap = mapMaybe
