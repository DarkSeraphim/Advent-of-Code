module Lib (Point, newPoint, x, y) where

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
