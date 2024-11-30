{-# LANGUAGE TupleSections #-}
module Day22.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Helpers.Point (Point, newPoint3, getX, getZ, getY, newPoint)
import Text.Parsec (char, endOfLine, sepEndBy1)
import Data.Map (fromList, Map, findWithDefault)
import qualified Data.Map as M
import Prelude hiding (lines)
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Maybe (mapMaybe, catMaybes)
import GHC.Exts (sortWith)
import Control.Monad (mfilter)

pComma :: Parser Char
pComma = char ','

pTilde :: Parser Char
pTilde = char '~'

pPoint :: Parser Point
pPoint = newPoint3 <$> (number <* pComma) <*> (number <* pComma) <*> number

pCube :: Parser Cube
pCube = (,) <$> pPoint <* pTilde <*> pPoint

pCubes :: Parser [Cube]
pCubes = pCube `sepEndBy1` endOfLine

type Cube = (Point, Point)
newtype Id = Id Int deriving (Eq, Ord, Show)
type HeightMap = Map Point (Map Int Id)

toXY :: Point -> Point
toXY p = newPoint (getX p) (getY p)

toMap :: (Id, Cube) -> HeightMap
toMap (i, c) = fromList (map (\p -> (toXY p, M.singleton (getZ p) i)) line)
  where line = expandLine c


expandX :: (Point, Point) -> [Point]
expandX (a, b) = map ((a+) . (\x -> newPoint3 x 0 0)) [0..getX b - getX a]

expandY :: (Point, Point) -> [Point]
expandY (a, b) = map ((a+) . (\y -> newPoint3 0 y 0)) [0..getY b - getY a]

expandZ :: (Point, Point) -> [Point]
expandZ (a, b) = map ((a+) . newPoint3 0 0) [0..getZ b - getZ a]

expandLine :: (Point, Point) -> [Point]
expandLine (a, b)
  | null points = error "Whoops inverse me please"
  | otherwise = points
  where points
          | getX a /= getX b = expandX (a, b)
          | getY a /= getY b = expandY (a, b)
          | otherwise = expandZ (a, b)

findHighestBelow :: HeightMap -> Id -> Point -> Maybe (Int, Id)
findHighestBelow heightMap i p = mfilter ((i /=) . snd) $ M.lookupLT (getZ p) (findWithDefault M.empty (toXY p) heightMap)

findSupports' :: HeightMap -> (Id, Cube) -> Set Id
findSupports' heightMap (i, c) = S.fromList (map (snd . snd)  supports)
  where line = expandLine c
        possible = mapMaybe (\p -> (p,) <$> findHighestBelow heightMap i p) line
        supports = filter (\(p, (z, _)) -> getZ p == z + 1) possible

findSupports :: HeightMap -> [(Id, Cube)] -> Set Id
findSupports heightMap cubes = S.unions definite
  where supports = map (findSupports' heightMap) cubes
        definite = filter (\s -> S.size s == 1) supports

findAllSupports :: HeightMap -> [(Id, Cube)] -> Map Id [Id]
findAllSupports heightMap cubes = M.fromList supports
  where supports = map (\(i, c) -> (i, S.toList $ findSupports' heightMap (i, c))) cubes

shiftCube :: Cube -> Int -> Cube
shiftCube (a, b) z = (a - dp, b - dp)
  where dz = getZ a - z
        dp = newPoint3 0 0 dz

toHeightMap :: HeightMap -> [(Id, Cube)] -> (HeightMap, [(Id, Cube)])
toHeightMap m [] = (m, [])
toHeightMap m ((i, c) : cs) = (m', (i, c') : cs')
  where line = expandLine c
        mabe = map (findHighestBelow m i) line
        newHeight = 1 + maximum (0 : map fst (catMaybes mabe))
        c' = shiftCube c newHeight
        (m', cs') = toHeightMap (M.unionWith M.union m (toMap (i, c'))) cs

sortKey :: Cube -> (Int, Int, Int, Int, Int, Int)
sortKey (a, b) = (getZ a, getZ b, getY a, getY b, getX a, getX b)

countTree :: Map Id [Id] -> Id -> Int
countTree m i
  | null next = 1
  | otherwise = 1 + sum (map (countTree m) next)
  where next = findWithDefault [] i m

solve :: IO ()
solve = do
  cubes <- parseInput pCubes
  let cubesWithIds' = zip (map Id [0..]) cubes
  let cubesWithIds = sortWith (sortKey . snd) cubesWithIds'
  let (heightMap, cubesWithIds'') = toHeightMap M.empty cubesWithIds
  let definiteSupports = S.toList $ findSupports heightMap cubesWithIds''
  printf "Cascade sum is %d\n" (length cubesWithIds - length definiteSupports)

