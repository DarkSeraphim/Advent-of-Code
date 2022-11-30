{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Day19.Part1 (solve) where
    import Text.ParserCombinators.Parsec
        ( GenParser, endBy1, sepBy1, char, parse, string, eof )
    import Text.Parsec.Char (endOfLine)

    import Helpers.Parsec (number)
    import Helpers.Input (orFail)
    import Text.Printf (printf)

    import Data.List (transpose, intersect, intercalate, nub, permutations, find)
    import Data.Set (fromList, Set, intersection, size, insert, member, empty, singleton)
    import Data.Map (singleton, unions, elems, fromList, toList, unionsWith, (!), Map, insert, notMember, keys)
    import Data.Maybe (catMaybes, mapMaybe)
    import qualified Control.Arrow as Data.Bifunctor

    import Debug.Trace (trace)
    
    type Point2D = (Int, Int)
    type Point = (Int, Int, Int)
    data Face = X | Y | Z
    data Beacon = Beacon {num :: Int, readings :: [Point]}

    instance Eq Beacon where
        a == b = num a == num b
    instance Ord Beacon where
        a <= b = num a <= num b

    parseInput :: GenParser Char st [Beacon]
    parseInput = beacon `sepBy1` endOfLine <* eof

    beacon :: GenParser Char st Beacon
    beacon = Beacon <$> header <*> points

    header :: GenParser Char st Int
    header = string "--- scanner " *> number <* string " ---" <* endOfLine

    points :: GenParser Char st [Point]
    points = ((,,) <$> (number <* comma) <*> (number <* comma) <*> number) `endBy1` endOfLine

    comma :: GenParser Char st Char
    comma = char ','

    rotate90' :: Int -> Point2D -> [Point2D]
    rotate90' 0 p = [p]
    rotate90' n p@(x, y) = p : rotate90' (n - 1) rot
        where rot = (y, -x)

    rotate90 :: Point2D -> [Point2D]
    rotate90 = rotate90' 3

    rotatePoint :: Face -> Point -> [Point]
    rotatePoint X (x, y, z) = map (\(y, z) -> (x, y, z)) $ rotate90 (y, z)
    rotatePoint Y (x, y, z) = map (\(x, z) -> (x, y, z)) $ rotate90 (x, z)
    rotatePoint Z (x, y, z) = map (\(x, y) -> (x, y, z)) $ rotate90 (x, y)

    rotatePoints :: [Point] -> [[Point]]
    rotatePoints points = x' ++ y'
        where x = transpose $ map (rotatePoint Z) points
              x' = concatMap (transpose . map (rotatePoint X)) x
              y = transpose $ map (rotatePoint Y) points
              y' = concatMap (transpose . map (rotatePoint X)) y


    getAllRotations :: Beacon -> [Beacon]
    getAllRotations Beacon {num, readings} = map (Beacon num) readings'
        where readings' = rotatePoints readings

    addPoint :: Point -> Point -> Point
    addPoint (a, b, c) (d, e, f) = (a + d, b + e, c + f)

    subPoint :: Point -> Point -> Point
    subPoint (a, b, c) (d, e, f) = (d - a, e - b, f - c)

    countSame :: [Point] -> [Point] -> (Point, Point) -> Int
    countSame a b (aa, bb) = length $ intersect aaa bbb
        where aaa = map (subPoint aa) a
              bbb = map (subPoint bb) b

    getOverlap :: Beacon -> Beacon -> Int
    getOverlap a b = maximum $ map (countSame aReadings bReadings) pairs
        where aReadings = readings a
              bReadings = readings b
              pairs = concatMap (\aa -> map (aa, ) bReadings) aReadings

    sorted :: Beacon -> Beacon -> (Int, Int)
    sorted a b
      | na < nb   = (na, nb)
      | otherwise = (nb, na)
        where na = num a
              nb = num b

    sameView :: Beacon -> Beacon -> Maybe (Beacon, [Beacon])
    sameView a b
      | num a == num b = Nothing -- Same view excludes the beacon itself, ironically (and yet not ironically)
      | otherwise = Just $ snd $ maximum (map (\p@(a, b) -> (getOverlap a b, (a, [b]))) allPairs)
        where allA = getAllRotations a
              allB = getAllRotations b
              allPairs = concatMap (zip allB . repeat) allA

    findView :: Beacon -> Beacon -> Maybe Beacon
    findView a b
      | a == b       = error "Shouldn't be"
      | overlap < 12 = Nothing
      | otherwise    = Just beacon
        where allB = getAllRotations b
              (overlap, beacon) = maximum $ map (\bb -> (getOverlap a bb, bb)) allB


    toString :: Point -> String
    toString (a, b, c) = printf "(%d, %d, %d)" a b c

    distance :: Point -> Point -> Int
    distance (a, b, c) (d, e, f) = abs (a - d) + abs (b - e) + abs (c - f)

    -- Filter 0 as that's the same beacon
    relDist :: Beacon -> Set Int
    relDist Beacon {readings} = Data.Set.fromList $ concatMap (\a -> filter (/= 0) $ map (distance a) readings) readings

    enoughOverlaps :: Set Int -> Set Int -> Bool
    enoughOverlaps a b
      | a == b = False
      | otherwise = size (intersection a b) >= 66

    -- Recursive BFS lets go!
    bfs :: [Beacon] -> Map Beacon Beacon -> Map Beacon [Beacon] -> (Map Beacon Beacon, [Beacon])
    bfs [] prev _ = (prev, [])
    bfs (current:rest) prev neighbours = (prev'', current : rotated)
        where new = neighbours ! current
              new' = mapMaybe (findView current) new
              new'' = filter (`notMember` prev) new'
              prev' = foldl (\map k -> Data.Map.insert k current map) prev new''
              (prev'', rotated) = bfs (rest ++ new'') prev' neighbours

    getOffsets :: [Point] -> (Int, Int, Int, Int, Int, Int)
    getOffsets [a, b, c, d] = (distance a b, distance a c, distance a d, distance b c, distance b d, distance c d)
    getOffsets _ = error "Not 4 beacons"

    makeResult :: Point -> Maybe [Point] -> Maybe (Point, Point)
    makeResult point Nothing = Nothing
    makeResult point (Just others) = Just (point, head others)

    perm3 :: Eq a => [a] -> [[a]]
    perm3 list = map snd $ filter fst tups
        where tups = concatMap (\d -> concatMap (\a -> concatMap (\b -> map (\c -> (a /= b && b /= c && a /= c, [a, b, c, d])) list) list) list) list

    findMatchingPoint :: Beacon -> Beacon -> (Point, Point)
    findMatchingPoint a b = head $ filter (\(pa, pb) -> length (map (subPoint pa) ra `intersect` map (subPoint pb) rb) >= 12) points
        where ra = readings a
              rb = readings b
              points = concatMap (\aa -> map (aa, ) rb) ra

    -- Make beacons of scanner b relative to scanner 0, based on scanner a
    relative :: (Point, Beacon) -> Beacon -> Point
    relative (aOrigin, a) b
      | num b == 0 = aOrigin
      | otherwise  = trace (printf "%s with %s" (toString aGood) (toString bGood)) $ addPoint aOrigin (subPoint bGood aGood)
        where (aGood, bGood) = findMatchingPoint a b
    relativeAll' :: Map Beacon Beacon -> Map Beacon Point -> [Beacon] -> [Beacon]
    relativeAll' _ _ [] = []
    relativeAll' prevs origins (next@Beacon {num, readings} : rest) = next' : relativeAll' prevs origins' rest
        where prev = prevs ! next
              prevOrigin = origins ! prev
              nextOrigin = relative (prevOrigin, prev) next
              next' = trace (printf "Beacon %d is at %s, prev was at %s" num (toString nextOrigin) (toString prevOrigin)) $ Beacon {num, readings = map (addPoint nextOrigin) readings}
              origins' = Data.Map.insert next nextOrigin origins

    relativeAll :: Map Beacon Beacon -> [Beacon] -> [Beacon]
    relativeAll prevs beacons = relativeAll' prevs (Data.Map.singleton (head beacons) (0,0,0)) beacons

    solve = do
        beacons <- orFail . parse parseInput "Input" =<< getContents

        let relDists = Data.Map.fromList $ map (\b -> (b, relDist b)) beacons
        let possibleNeighbours = unionsWith (++) $ concatMap (\a -> map (\b -> Data.Map.singleton a [b]) $ filter (\b -> enoughOverlaps (relDists ! a) (relDists ! b) ) beacons) beacons

        let first = head beacons
        let (prev, rotated) = bfs [first] (Data.Map.singleton first first) possibleNeighbours
        let good = relativeAll prev rotated
        let unique = nub $ concatMap readings good
        printf "Hello beacons %d\n" (length unique)
