module Lecture9 where


-- -- Any regular expression is a condition on string
-- type RegExp = String -> Bool

-- -- Empty string
-- epsilon :: RegExp
-- epsilon = (== "")

-- char :: Char -> RegExp
-- char ch = (== [ch])

-- (|||) :: RegExp -> RegExp -> RegExp
-- e1 ||| e2 = \x -> e1 x || e2 x

-- (<*>) :: RegExp -> RegExp -> RegExp
-- e1 <*> e2 = \x ->
--   or [e1 y && e2 z | (y, z) <- splits x]
-- -- splits :: String -> [(String, String)]
-- -- returns all the ways a string can be split
-- -- in two

-- -- or is used to check whether particular
-- -- split satisfies the pattern or not

-- star :: RegExp -> RegExp
-- star p = epsilon ||| (p Lecture9.<*> star p)

-- defining a constant function in haskell

-- :t const ~> const :: a -> b -> a

const_0 :: a -> Integer
const_0 = const 0

-- Folding to the left:
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- Example:
-- foldr (+) 0 [1, 2, 3] => 3 + (2 + (1 + 0))
-- foldl (+) 0 [1, 2, 3] => ((0 + 1) + 2) + 3

-- Difference between them, when using non-associative
-- operators

-- foldr (^) 2 [1..3] == 1
-- foldl (^) 2 [1..3] == 64

-- Equivalence between folds
-- foldr f z xs = foldl (flip f) z (reverse xs)

-- Scan functions!

-- Combination of mapping and folding that produces
-- all the intermediate results of folding as a list

-- Scanning to the right:
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- scanr (+) 0 [1..5] ~> [15,14,12,9,5,0]


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f xs = foldr (\x acc -> if f x then x : acc else []) [] xs
