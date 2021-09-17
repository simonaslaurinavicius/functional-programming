{-
  Solutions.hs

  Solutions to the Problem set 1 for the
  Functional Programming course at Vilnius University

  Author: Simonas Laurinavicius
  Study program, course, group: Informatics, 4, 1
  Email: simonas.laurinavicius@mif.stud.vu.lt
  Last updated: 2021-09-17
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
nAnd_3 False False = True
nAnd_3 True False = True
nAnd_3 False True = True
nAnd_3 True True = False

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
prop_nAnds_2 =
  (nAnd True True && nAnd_2 True True && nAnd_3 True True) == False

-- Test can be done in GHCi using `quickCheck prop_nAnds(_2)` command

{-
  Exercise 3 -
    define function nDigits, which takes any integer number
    and returns the number of its digits.
-}

nDigits :: Integer -> Int
nDigits n
  | n >= 0 = (length . show) n
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

