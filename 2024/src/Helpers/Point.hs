module Helpers.Point (Point, PointBig, newPoint, newPoint3, getX, getY, getZ, manhattan, neighbours, neighbours3) where
data PointA a = PointA {x :: a, y :: a, z :: a} deriving (Show, Eq, Ord)

type Point = PointA Int
type PointBig = PointA Integer

instance Num a => Num (PointA a) where
  (+) a b = PointA (x a + x b) (y a + y b) (z a + z b)
  (*) a b = PointA (x a * x b) (y a * y b) (z a * z b)
  abs a = PointA (abs $ x a) (abs $ y a) (abs $ z a)
  signum a = PointA (signum $ x a) (signum $ y a) (signum $ z a)
  fromInteger _ = error "Not implemented"
  negate a = PointA (negate $ x a) (negate $ y a) (negate $ z a)

newPoint :: Num a => a -> a -> PointA a
newPoint x' y' = PointA { x = x', y = y', z = 0}

newPoint3 :: a -> a -> a -> PointA a
newPoint3 x' y' z' = PointA { x = x', y = y', z = z'}

getX :: PointA a -> a
getX = x
getY :: PointA a -> a
getY = y
getZ :: PointA a -> a
getZ = z

manhattan :: Num a => PointA a -> PointA a -> a
manhattan a b = x c + y c + z c
  where c = abs (a - b)

neighbours :: [Point]
neighbours = map (uncurry newPoint) [(0, 1), (1, 0), (0, -1), (-1, 0)]

neighbours3 :: [Point]
neighbours3 = map (\(x', y', z') -> newPoint3 x' y' z')
  [
    (1, 0, 0), (-1, 0, 0),
    (0, 1, 0), (0, -1, 0),
    (0, 0, 1), (0, 0, -1)
  ]
