{-
  Solutions.hs

  Solutions to the Problem set 4 for the
  Functional Programming course at Vilnius University

  Author: Simonas Laurinavicius
  Study program, course, group: Informatics, 4, 1
  Email: simonas.laurinavicius@mif.stud.vu.lt
  Last updated: 2021-12-12
-}

module Solutions where

import Data.List (genericLength)
import qualified Data.Set as Set

{-
  Exercise 1

    Instead of binary trees, we can define general trees
    with an arbitrary list of subtrees, e.g.,

        data GTree a = Leaf a | Gnode [GTree a]

    Define functions for this datatype, which
        - return the depth of a GTree;
        - find whether an element occurs in a GTree;
        - map a given function over the elements at the leaves of a GTree
        (i.e., a variation of the mapTree function from the Lecture 11 slides for GTree).
-}

data GTree a = Leaf a | Gnode [GTree a]
    deriving (Show)

treeDepth :: GTree a -> Integer
treeDepth (Leaf _) = 0
treeDepth (Gnode xs) = 1 + maximum [treeDepth subtree | subtree <- xs]

treeOccurs :: Eq a => GTree a -> a -> Bool
treeOccurs (Leaf x) y = x == y
treeOccurs (Gnode xs) y = or [treeOccurs subtree y | subtree <- xs]

mapTree :: (a -> b) -> GTree a -> GTree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Gnode xs) = Gnode [mapTree f subtree | subtree <- xs]

{-
  Exercise 2

    We can define a data structure for generalised expressions as follows:

        data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
        type Ops a = [a] -> a
        type Var = Char

    Thus, an expression can be either a literal (of the type a),
    a variable name, or an operator applied to a list of expressions.

    In turn, an operator is represented as a function that reduces a
    list of a values to a single result.
    To evaluate an expression, we need to know all the current values
    (of the type a) associated to the expression variables.

    Define a new type Valuation a, relating variables (of the type Var)
    with the values of the type a.

    Then write a function:

          eval ::  Valuation a -> Expr a -> a

    that, for the given variable valuation and expression,
    evaluates (folds) the expression to a single value.
-}

data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [a] -> a
type Var = Char
type Valuation a = Var -> a

eval :: Valuation a -> Expr a -> a
eval _ (Lit a) = a
eval symTable (EVar var) = symTable var
eval symTable (Op f xs) = f [eval symTable expr | expr <- xs]

{- Evaluating a simple program -}
symbolTable :: Valuation Integer
symbolTable 'a' = 1
symbolTable 'b' = 2
symbolTable 'c' = 3
symbolTable _ = error "undefined variable!"

program :: Expr Integer
program = Op sum [Lit 1, Lit 2, EVar 'b']

{-
  Exercise 3

    Extend the regular expression example (see the slides of Lecture 9)
    by defining the functions:

        option, plus :: RegExp -> RegExp

    where option p matches zero or one occurrences of the pattern p,
    and plus p matches one or more occurrences of the pattern p.
-}

type RegExp = String -> Bool

{- Match empty string -}
epsilon :: RegExp
epsilon = (== "")

{- Match single letter -}
char :: Char -> RegExp
char ch = (== [ch])

{- Alternate between patterns -}
(|||) :: RegExp -> RegExp -> RegExp
p1 ||| p2 = \x -> p1 x || p2 x

{- Match patterns to all subsets of the string -}
(<*>) :: RegExp -> RegExp -> RegExp
p1 <*> p2 = \x ->
  or [p1 y && p2 z | (y, z) <- splits x]

splits :: [a] -> [([a], [a])]
splits xs = [splitAt x xs | x <- [0..(length xs)]]

{- Match 0 or more times -}
star :: RegExp -> RegExp
star p = epsilon ||| (p Solutions.<*> star p)

{- Match 0 or 1 time -}
option :: RegExp -> RegExp
option p = epsilon ||| p

{- Match 1 or more times -}
plus :: RegExp -> RegExp
plus p = p Solutions.<*> star p

{-
  Exercise 4

    Define a new data type

        NumList a = NList [a],

    for lists containing numbers.

    Make this type an instance of the type classes Eq and Ord by defining
    that such lists are equal only if their average values are equal, and
    such lists can be compared only by comparing their respective average values.

    Add the needed context (type class dependencies) to make such instances work.
    The average value for [] is 0.
-}

data NumList a = NList [a]
    deriving (Show)

{-
    :info Eq output:

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        {-# MINIMAL (==) | (/=) #-}

    MINIMAL denotes the operations that comprise the minimal
    definition requirement - there must be a concrete definition
    for at least one of the operators specified.
-}

instance Real a => Eq (NumList a) where
    (==) (NList xs) (NList ys) = average xs == average ys

instance Real a => Ord (NumList a) where
    (<=) (NList xs) (NList ys) = average xs <= average ys

{-
    Solution from:
    https://stackoverflow.com/questions/2376981/haskell-types-frustrating-a-simple-average-function
-}
average :: (Real a, Fractional b) => [a] -> b
average [] = 0
average xs = realToFrac (sum xs) / genericLength xs

{-
  Exercise 5

    The Maybe a type could be generalised to allow messages to
    be carried in the Nothing part, e.g.,

    data Result a = OK a | Error String

    Define the function

        composeResult ::  (a -> Result b) -> (b -> Result c) -> (a -> Result c)

    which composes together two (possibly error-raising) functions.
-}

data Result a = OK a | Error String
    deriving (Show)

composeResult :: (a -> Result b) -> (b -> Result c) -> (a -> Result c)
composeResult f g = g' . f
    where
        g' (Error message) = Error ("Error occured in f" ++ message)
        g' (OK a) = g a

fun1 :: (a -> Result a)
fun1 _ = Error "error message"
-- fun1 x = OK x

fun2 :: (a -> Result a)
fun2 = OK

{-
    Exercise 6
        The Goldbach conjecture for prime numbers (still unproven in the general case)
        states that any even number greater than 2 can be rewritten as a sum of
        two prime numbers.

        Write a function that checks that the conjecture is true up to the given upper bound.
        In other words, the function

                  goldbach ::  Integer -> Bool

        for the given integer n, should return True if all even numbers
        in [4..n] satisfy the conjecture.

        Use the generated infinite list of primes (from the Lecture 12 slides)
        and list comprehensions to construct and compare
        the corresponding lists of numbers (e.g., those that can be expressed as
        sums of two primes and those that are even).
-}

{- Generating primes using Sieve of Eratosthenes -}
primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]

goldbach :: Integer -> Bool
goldbach n = length even' == length satisfies
    where
        even' = [4,6..n]
        satisfies = Set.fromList [x + y | x <- primes', y <- primes', elem (x + y) even']
        primes' = takeWhile (<= n) primes

{-
    Exercise 7
        Infinite data streams, i.e., the lists that must be infinite,
        can be defined in Haskell as follows:

            data Stream a = Cons a (Stream a)

    Define the described below functions for this datatype:

        - streamtoList :: Stream a -> [a]
        creates an infinite list out of the given stream;

        - streamIterate :: (a -> a) -> a -> Stream a
        creates a stream for the given iteration function and the starting stream element (seed);

        - streamInterleave :: Stream a -> Stream a -> Stream a
        merges two streams into one so that their elements are interleaved.
        In other words, for two given streams ⟨e11, e12, e13, ...⟩ and
        ⟨e21, e22, e23, ...⟩, the result would be the stream ⟨e11, e21, e12, e22, ...⟩
-}

data Stream a = Cons a (Stream a)
    deriving (Show)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x stream1) stream2 =
    Cons x (streamInterleave stream2 stream1)

{- Stream examples -}
nats :: Integer -> Stream Integer
nats n = Cons n (nats (n + 1))

praise :: Stream String
praise = Cons "Praise" praise

religiousFigures :: Integer -> Stream String
religiousFigures n = Cons (figures !! fromIntegral (n `mod` z)) (religiousFigures (n + 1))
    where
        figures = ["Jesus Christ", "Buddha", "Muhammad"]
        z = fromIntegral (length figures)

{-
    Usage:

    take 5 (streamToList (streamIterate (*2) 5))
    take 10 (streamToList (streamInterleave praise (religiousFigures 0)))
-}
