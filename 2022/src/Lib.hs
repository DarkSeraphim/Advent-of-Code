module Lib (Point, newPoint, x, getX, y, getY, manhattan, Range, newRange, merge, includes, overlaps, compress) where

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)

instance Num Point where
  (+) a b = Point (x a + x b) (y a + y b)
  (*) a b = Point (x a * x b) (y a * y b)
  abs a = Point (abs $ x a) (abs $ y a)
  signum a = Point (signum $ x a) (signum $ y a)
  fromInteger a = error "Not implemented"
  negate a = Point (negate $ x a) (negate $ y a)

newPoint :: Int -> Int -> Point
newPoint x y = Point { x = x, y = y }

getX = x
getY = y

manhattan :: Point -> Point -> Int
manhattan a b = x c + y c
  where c = abs (a - b)

data Range a = Range a a deriving (Eq, Ord, Show)
newRange :: a -> a -> Range a
newRange = Range

getMin :: Range a -> a
getMin (Range a b) = a

getMax :: Range a -> a
getMax (Range a b) = b

includes :: Ord a => Num a => a -> Range a -> Bool
includes n (Range a b) = a <= n && n <= b

overlaps :: Ord a => Num a => Range a -> Range a -> Bool
overlaps r1@(Range a b) r2@(Range x y) = includes a r2 || includes b r2 || includes x r1 || includes y r2

merge :: Ord a => Num a => Range a -> Range a -> Maybe (Range a)
merge (Range a b) (Range x y)
  | b < x - 1 = Nothing
  | otherwise = Just $ Range (min a x) (max b y)

compress :: Ord a => Num a => [Range a] -> [Range a]
compress [] = []
compress [a] = [a]
compress (a:b:rest) = 
  case c of
    Just c' -> compress (c':rest)
    Nothing -> a : compress (b:rest)
  where c = merge a b



