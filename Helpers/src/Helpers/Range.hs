module Helpers.Range (Range, newRange, getMin, getMax, merge, includes, overlaps, intersectLeft, contains, compress, translate, size) where

data Range a = Range a a deriving (Eq, Ord, Show)
newRange :: a -> a -> Range a
newRange = Range

getMin :: Range a -> a
getMin (Range a _) = a

getMax :: Range a -> a
getMax (Range _ b) = b

includes :: Ord a => a -> Range a -> Bool
includes n (Range a b) = a <= n && n <= b

overlaps :: Ord a => Range a -> Range a -> Bool
overlaps r1@(Range a b) r2@(Range c d) = includes a r2 || includes b r2 || includes c r1 || includes d r1

contains :: Ord a => Range a -> Range a -> Bool
contains (Range a b) (Range c d) = a >= c && b >= d

intersectLeft :: Ord a => Range a -> Range a -> Maybe (Range a)
intersectLeft r1@(Range a b) r2@(Range c d)
  | overlaps r1 r2 = Just $ newRange (max a c) (min b d)
  | otherwise = Nothing

merge :: Ord a => Num a => Range a -> Range a -> Maybe (Range a)
merge (Range a b) (Range c d)
  | b < c - 1 = Nothing
  | otherwise = Just $ Range (min a c) (max b d)

compress :: Ord a => Num a => [Range a] -> [Range a]
compress [] = []
compress [a] = [a]
compress (a:b:rest) =
  case c of
    Just c' -> compress (c':rest)
    Nothing -> a : compress (b:rest)
  where c = merge a b

translate :: Num a => a -> Range a -> Range a
translate n (Range a b) = newRange (a + n) (b + n)

size :: Num a => Range a -> a
size r = getMax r - getMin r + 1
