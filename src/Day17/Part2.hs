module Day17.Part2 (solve) where
  import Data.List (intercalate)
  import Data.Set (fromList, size, Set, delete, member, toList)
  import Text.Printf (printf)
  import qualified Debug.Trace
  import Debug.Trace (trace)

  pack :: (Int, (Int, Char)) -> [[Int]]
  pack (row, (column, '#')) = [[row, column, 0, 0]]
  pack (row, (column, _)) = []

  parseList :: (Int, String) -> [[Int]]
  parseList (idx, line) = concatMap pack $ zip (repeat idx) $ zip [0..] line

  parse = do
    ls <- lines <$> getContents
    let lineWithIndex = zip [0..] ls
    let cubes = map parseList lineWithIndex
    return cubes

  getNeighbours' :: [Int] -> [Int] -> [Int] -> [[Int]]
  getNeighbours' origin neighbour []
    | origin == reverse neighbour = []
    | otherwise           = [reverse neighbour]
  getNeighbours' origin neighbour (x:xs) = concatMap ((\x -> getNeighbours' origin x xs) . ((: neighbour) . (+x))) [-1, 0, 1]
  getNeighbours origin = n2
    where n = getNeighbours' origin [] origin
          n2 = trace ("Neighbours found: " ++ show (length n)) n

  isActive oldSet cube
    | nowActive && len == 2 = True
    | len == 3 = True
    | otherwise = False
    where len = length $ filter (`member` oldSet) $ getNeighbours cube
          nowActive = member cube oldSet

  getActive :: Set [Int] -> Set [Int]
  getActive set = fromList $ filter (isActive set) $ concatMap getNeighbours set ++ toList set

  runSimulation :: Int -> Set [Int] -> Set [Int]
  runSimulation 0 set = set
  runSimulation i set = runSimulation (i - 1) active2
    where active = getActive set
          active2 = trace("Intermediate length is " ++ show (size set)) active

  solve = do
    d <- fromList . concat <$> parse
    let cubeSet = runSimulation 6 d
    printf "Number of active cubes is %d \n" (size cubeSet)
