module Lib (Point, PointBig, newPoint, newPoint3, getX, getY, getZ, manhattan, neighbours, neighbours3, Range, newRange, merge, includes, overlaps, compress) where

data PointA a = PointA {x :: a, y :: a, z :: a} deriving (Show, Eq, Ord)

type Point = PointA Int
type PointBig = PointA Integer

instance Num a => Num (PointA a) where
  (+) a b = PointA (x a + x b) (y a + y b) (z a + z b)
  (*) a b = PointA (x a * x b) (y a * y b) (z a * z b)
  abs a = PointA (abs $ x a) (abs $ y a) (abs $ z a)
  signum a = PointA (signum $ x a) (signum $ y a) (signum $ z a)
  fromInteger a = error "Not implemented"
  negate a = PointA (negate $ x a) (negate $ y a) (negate $ z a)

newPoint :: Num a => a -> a -> PointA a
newPoint x y = PointA { x = x, y = y, z = 0}

newPoint3 :: Num a => a -> a -> a -> PointA a
newPoint3 x y z = PointA { x = x, y = y, z = z}

getX = x
getY = y
getZ = z

manhattan :: Num a => PointA a -> PointA a -> a
manhattan a b = x c + y c + z c
  where c = abs (a - b)

neighbours :: [Point]
neighbours = map (uncurry newPoint) [(0, 1), (1, 0), (0, -1), (-1, 0)]

neighbours3 :: [Point]
neighbours3 = map (\(x, y, z) -> newPoint3 x y z) 
  [
    (1, 0, 0), (-1, 0, 0),
    (0, 1, 0), (0, -1, 0),
    (0, 0, 1), (0, 0, -1)
  ]

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



