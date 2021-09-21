{-
  Solutions.hs

  Solutions to the Problem set 1 for the
  Functional Programming course at Vilnius University

  Author: Simonas Laurinavicius
  Study program, course, group: Informatics, 4, 1
  Email: simonas.laurinavicius@mif.stud.vu.lt
  Last updated: 2021-09-20
-}

module Solutions where

import Test.QuickCheck

{-
  Exercise 1

    Define three versions of nAnd function
    version 1 by using boolean operators and
    version 2 by using simple pattern matching
    version 3 by encoding a truth table for the function
-}

nAnd :: Bool -> Bool -> Bool
nAnd x y = not (x && y)

nAnd_2 :: Bool -> Bool -> Bool
nAnd_2 True x = not x
nAnd_2 False _ = True

nAnd_3 :: Bool -> Bool -> Bool
nAnd_3 True True = False
nAnd_3 _ _ = True

{-
  Dumb truth table
    nAnd_3 False False = True
    nAnd_3 True False = True
    nAnd_3 False True = True
    nAnd_3 True True = False
-}

{-
  Exercise 2 -
    verify that nAnd definitions are functionally equivalent
    using quickCheck library.
-}

-- Defining a property
prop_nAnds :: Bool -> Bool -> Bool
prop_nAnds x y =
  nAnd x y == nAnd_2 x y && nAnd_2 x y == nAnd_3 x y

-- Defining an additional property
prop_nAnds_2 :: Bool
prop_nAnds_2 = nAnd True True == False

-- Test can be done in GHCi using `quickCheck prop_nAnds(_2)` command

{-
  Exercise 3 -
    define function nDigits, which takes any integer number
    and returns the number of its digits.
-}

nDigits :: Integer -> Int
nDigits n
  | n >= 0 = length (show n)
  | otherwise = (length . show) (-n)

{-
  Exercise 4 -
    define a function nRoots :: Float −> Float −> Float −> Int which
    returns the number of solutions for a quadratic equation
-}

nRoots :: Float -> Float -> Float -> Int
nRoots a b c
  | a == 0 = error "the first argument should be non-zero!"
  | b ^ 2 > 4 * a * c = 2
  | b ^ 2 == 4 * a * c = 1
  | b ^ 2 < 4 * a * c = 0

{-
  Exercise 5 -
    define the functions smallerRoot :: Float −> Float −> Float −> Float
    and largerRoot :: Float −> Float −> Float −> Float, which respectively
    return the smaller and larger root of a quadratic equation,
    for the given real coefficients a, b, and c.
-}

type Roots = (Float, Float)

rootHelper :: (Roots -> Float) -> Float -> Float -> Float -> Float
rootHelper extrema a b c
  | nRoots a b c == 0 = error "there are no roots for the coefficients!"
  | nRoots a b c == 1 = fst (roots a b c)
  | nRoots a b c == 2 = extrema (roots a b c)

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = rootHelper minimum a b c

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = rootHelper maximum a b c

roots :: Float -> Float -> Float -> Roots
roots a b c =
  (((-b) + sqrt discriminant) / (2 * a),
  ((-b) - sqrt discriminant) / (2 * a))
  where
    discriminant = b ^ 2 - 4 * a * c


{-
  Exercise 6 -
    write a function power2 :: Integer -> Interger that calculates
    the power of 2 for the given natural number using primitive recursion
    mechanism.
-}

power2 :: Integer -> Integer
power2 n
  | n == 0 = 1
  | n > 0 = 2 * power2 (n - 1)
  | otherwise = error "negative argument!"

{-
  Exercise 7 -
    Write a function mult :: Integer −> Integer −> Integer
    that recursively redefines the multiplication operation
    by using only addition.
-}

mult :: Integer -> Integer -> Integer
mult m n
  | m >= 1 = n + mult (m - 1) n
  | m < 0 = - mult (-m) n
  | otherwise = 0

{-
  Exercise 8 -
    Define a recursive function prod :: Integer −> Integer −> Integer
    that, for the given numbers m and n,
    multiplies all the numbers from the range m...n.
-}

prod :: Integer -> Integer -> Integer
prod m n
  | n == m = n
  | n > m = m * prod (m + 1) n
  | otherwise = error "invalid range, first parameter must be less than second!"

{-
  redefine factorial function:
    fact :: Integer -> Integer
    fact n
      | n == 0 = 1
      | n > 0 = fact(n - 1) * n

    as a special case of prod
-}

fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | n > 0 = prod 1 n
