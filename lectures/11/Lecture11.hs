module Lecture11 where


-- User-defined datatypes revisited

-- data TypeName =
-- Con_1 t11 .. t1k
-- ...
-- Con_n tn1 .. tnk_n

-- Con_i - constructors, tij - type names

-- Constructor is a function that takes arguments
-- of the types ti1 .. tik and returns a result of the type TypeName

-- Possible extensions: recursive and/or polymorphic datatypes.

-- Recursive datatypes

data Expr = Literal Integer |
  Add Expr Expr |
  Sub Expr Expr
  deriving (Show)

example :: Expr
example = Add (Sub (Literal 17) (Literal 15)) (Literal 11)


data NTree = NilT |
  NNode Integer NTree NTree
  deriving (Show)
-- a tree is either nil or is given by combining a value and two subtrees

example2 :: NTree
example2 = NNode 10 NilT NilT

-- Writing functions (based on primitive recursion) for a recursive datatype
eval :: Expr -> Integer

eval (Literal n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

depth :: NTree -> Integer
depth NilT = 0
depth (NNode _ t1 t2) = 1 + max (depth t1) (depth t2)

occurs :: NTree -> Integer -> Integer
occurs NilT _ = 0
occurs (NNode n t1 t2) m
  | n == m = 1 + occurs t1 m + occurs t2 m
  | otherwise = occurs t1 m + occurs t2 m

-- Mutually recursive types - use other types when creating a new datatype

-- data Person = Adult Name Address Bio | Child Name

-- data Bio = Parent String [Person] | NonParent String

-- Polymorphic datatypes - type variables occur in datatype descriptions
data Pairs a = Pair a a
  deriving (Show, Eq)

equalPair :: Eq a => Pairs a -> Bool
equalPair (Pair x y) = x == y

example3 :: Pairs Integer
example3 = Pair 4 2

example4 :: Pairs String
example4 = Pair "Hello" "World!"


-- Redefining definition of list to use ':::' for list
-- combining.

infixr 5 :::

data MyList a = NilL | a ::: (MyList a)
  deriving(Eq, Ord, Show, Read)

-- Here fixity declaration indixr 5 ::: defines the fixity strength - priority (5)
-- from the range 0..9 and the kind of associativity (right - r) for the :::
-- operator. The values are the same as it is defined for the predefined
-- operator : .

example5 :: MyList Integer
example5 = 1 ::: 2 ::: NilL

-- Polymorphic binary tree

data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Read)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node x t1 t2) =
  collapse t1 ++ [x] ++ collapse t2

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Nil = Nil
mapTree f (Node x t1 t2) =
  Node (f x) (mapTree f t1) (mapTree f t2)


-- Either datatype

-- data Either a b = Left a | Right b
--   deriving (Eq, Ord, Show, Read)

-- f :: Either a b -> Int
-- f (Left x) = x
-- f (Right y) = y

-- Higher-order function on Either a b
-- Specify both funtion for a and function for b
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x) = f x
either f g (Right y) = g y

-- Errors in Haskell!

-- 1. Generating exceptions - error :: String -> a function
-- 2. Returning a pre-defined (dummy) value
--   dangerous - adds overhead on how to distinguish different return values
-- 3. Error datatypes, such as Maybe (beautiful solution!)

-- data Maybe a = Nothing | Just a
--   deriving (Eq, Ord, Read, Show)

maybeExample :: Integer -> Integer -> Maybe Integer
maybeExample n m
  | (m /= 0) = Just (div n m)
  | otherwise = Nothing

-- Dealing with Maybe datatypes:
-- We can either transmit error further, trap it, or raise an error

-- Example of transmitting the error
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

-- Building more complex datatypes usually goes hand in hand
-- with defining new type classes for these new complex datatypes

data Vector = Vec Float Float

class Movable a where
  move :: Vector -> a -> a
  reflectX :: a -> a
  reflectY :: a -> a
  reflect180 :: a -> a
  reflect180 = reflectX . reflectY

data Point = Point Float Float
  deriving(Show)

instance Movable Point where
  move (Vec v1 v2) (Point c1 c2) =
    Point (c1 + v1) (c2 + v2)
  reflectX (Point c1 c2) = Point c1 (-c2)
  reflectY (Point c1 c2) = Point (-c1) c2
  -- No need for this as this is the same as default
  -- reflect180 (Point c1 c2) = Point (-c1) (-c2)

-- Modules

-- Visibility restrictions can be given either in a module
-- declaration (module M (...)) or during module import (import M (...),
-- import M hiding (...))


-- module Ant where
--   data Ants = ...
--   antEater x = ...


-- module Bee where
--   import Ant

-- beeKeeper x = ...
-- All from Ant can be used here

-- Controlling what is exported

-- module Bee (beeKeeper, Ants(..), antEater) where
--   ...

-- Here TypeName(..) denotes that all the constructors of Ants
-- are exported with the datatype

-- If (..) is omitted, then the datatype acts as ADT and can be only
-- accessed by provided operations inside module declaration

-- see BST example below

-- Each


-- Abstract datatypes in Haskell

-- Abstract datatype: has a clearly defined and agreed interface
-- (signature of ADT), allowing

-- Binary search tree is an object of type Tree a
data Tree a = Nil | Node a (Tree a) (Tree a)

-- whose elements are ordered

-- The tree (Node val t1 t2) is ordered if
  -- all vlaues in t1 are smaller than val
  -- all values in t2 are larger than val
  -- the trees t1 and t2 are themselves ordered.

-- The operations for building and manipulating such tree must preserve
-- the order. We can ensure that only such approved operations are used
-- by making this datatype into ADT.

module BST
 (Tree,
  nil, -- Tree a
  isNil, -- Tree a -> Bool
  isNode, -- Tree a -> Bool
  leftSub, -- Tree a -> Maybe (Tree a)
  rightSub, -- Tree a -> Maybe (Tree a)
  treeVal, -- Tree a -> Maybe a
  insertVal, -- Ord a => a -> Tree a -> Tree a
  deleteVal, -- Ord a => a -> Tree a -> Tree a
  minTree -- Ord a => Tree a -> Maybe a
  )
where

-- The constructors for Tree datatype are hidden!
