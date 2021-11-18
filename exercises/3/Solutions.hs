{-
  Solutions.hs

  Solutions to the Problem set 3 for the
  Functional Programming course at Vilnius University

  Author: Simonas Laurinavicius
  Study program, course, group: Informatics, 4, 1
  Email: simonas.laurinavicius@mif.stud.vu.lt
  Last updated: 2021-11-17
-}

module Solutions where

{-
  Exercise 1

    Extend the Shape datatype definition by, for each shape,
    adding it's position ((x, y) coordinates) as an extra
    argument or arguments.

    Write a function
      overlaps:: Shape -> Shape -> Bool

    that checks whether two given shapes are overlapping or not.
-}

data Shape = Circle Float Position | Rectangle Float Float Position
     deriving (Show, Ord, Eq)

type Position = (Float, Float)

overlaps :: Shape -> Shape -> Bool
overlaps (Circle r1 (x1, y1)) (Circle r2 (x2, y2)) =
  sqrt((x1 - x2)^2 + (y1 - y2)^2) < r1 + r2

-- Algorithm referenced from: https://stackoverflow.com/a/1879223
overlaps (Circle r (x1, y1)) (Rectangle h w (x2, y2)) = distance < r
  where
    distance = sqrt((x1 - cx)^2 + (y1 - cy)^2)
    cx = clamp rLeft rRight x1
    cy = clamp rTop rBottom y1
    (rLeft, rRight) = (x2 - (w/2), x2 + (w/2))
    (rTop, rBottom) = (y2 + (h/2), y2 - (h/2))

overlaps a@(Rectangle h w (x1, y1)) b@(Circle r (x2, y2)) =
  overlaps b a

-- Great rectangle intersection visualisation - https://silentmatt.com/rectangle-intersection/
overlaps (Rectangle h1 w1 (x1, y1)) (Rectangle h2 w2 (x2, y2)) =
  (r1Left < r2Right) && (r1Right > r2Left) && (r1Top > r1Bottom) && (r1Bottom < r1Top)
  where
    (r1Left, r1Right) = (x1 - (w1/2), x1 + (w1/2))
    (r1Top, r1Bottom) = (y1 + (h1/2), y1 - (h1/2))
    (r2Left, r2Right) = (x2 - (w2/2), x2 + (w2/2))
    (r2Top, r2Bottom) = (y2 + (h2/2), y2 - (h2/2))

clamp :: Float -> Float -> Float -> Float
clamp min' max' = max min' . min max'

{-
  Exercise 2

      Define your own versions for the standard functions:

      any :: (a->Bool) -> [a] -> Bool
      all :: (a->Bool) -> [a] -> Bool

      which test whether some or all list elements satisfy the given property.
      Please provide two versions, one relying on filter (very easy),
      and the other one relying on map and foldr.
-}

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = not (null (filter f xs))

all' :: Eq a => (a -> Bool) -> [a] -> Bool
all' f xs = filter f xs == xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' f = foldr ((||) . f) False

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = foldr ((&&) . f) True

{-
  above function with map, if lecturer asks -
    any'' f = foldr (||) False (map f xs)
-}

{-
  Exercise 3

    Redefine the standard function

    unzip :: [(a,b)] -> ([a],[b])

    which unzips a list of pairs into a pair of the
    corresponding lists, using the foldr function.
-}

unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr f ([],[])
  where
    f (x,y) (xs,ys) = (x:xs, y:ys)

-- Example => unzip' [(1,'a'),(2,'b'),(3,'c')]

{-
  Exercise 4

    Redefine the standard function

    length :: [a] -> Int

    using map, function composition (.),
    and, if possible, lambda abstraction.

    Also, write an alternative version of this function,
    which is based on folding (e.g., using foldr).
-}

length' :: [a] -> Int
length' = sum . map (const 1)

length'' :: [a] -> Int
length'' = foldr (\_ acc -> acc + 1) 0

{-
  Exercise 5

    Write a function

    ff :: Integer -> [Integer] -> Integer

    which filters the given list (the second argument) by removing the negative numbers,
    then multiplying each number by 10, and finally adding list numbers together while
    their sum is not exceeding the given bound (the first argument).
    The function should be defined as functional composition (.)
    of the respective functions implementing the described actions.
-}

ff :: Integer -> [Integer] -> Integer
ff upperBound = sumUntil . map (* 10) . filter (>= 0)
  where
    sumUntil = foldr (\x acc -> if acc < upperBound then acc + x else acc) 0

{-
  Example where result with foldl differs

    We have to use
    ff 100 [9,2,2,2,2,2]

    With foldr - we get 100, as we start from the right,
    with foldl - we get 110, as we start from the left

    sumUntil = foldl (\acc x -> if acc < upperBound then acc + x else acc) 0
-}

{-
  Exercise 6

    Write a function

    total :: (Integer -> Integer) -> Integer -> Integer

    so that total f is the function which, for the given value n, returns

    f0 + f1 + ... + fn

    Your solution must rely either on applying map and
    functional composition or using folding (e.g., foldr).
-}

total :: (Integer -> Integer) -> Integer -> Integer
total f n
  | n < 0 = error "negative number provided!"
  | otherwise = foldl (\acc x -> acc + f x) 0 [0..n]


total' :: (Integer -> Integer) -> Integer -> Integer
total' f n
  | n < 0 = error "negative number provided!"
  | otherwise = (sum . map f) [0..n]

iter' :: Integer -> (a -> a) -> (a -> a)
iter' n f
  | n <= 0 = id
  | otherwise = f . iter' (n - 1) f

iter'' :: Integer -> (a -> a) -> (a -> a)
iter'' n f = foldr (.) id functions
  where
    functions = replicate (fromIntegral n) f

splits :: [a] -> [([a], [a])]
splits xs = [splitAt' x xs | x <- [0..(length xs)]]

-- Implementation of Prelude splitAt
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)
