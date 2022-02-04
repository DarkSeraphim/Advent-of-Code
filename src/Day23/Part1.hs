{-# LANGUAGE TupleSections #-}
module Day23.Part1 (solve) where
    import Text.Printf (printf)
    import Data.Maybe (catMaybes)
    import Data.Map (fromList, Map, toList, (!), delete, insert, empty, findWithDefault, elems, lookup, singleton)
    import Data.Set (Set, empty, notMember, insert, toList, singleton, deleteFindMin, size)
    import Data.List (nub, intercalate)
    import Debug.Trace (trace)

    type Point = (Int, Int)
    data AmphipodId = A | B | C | D
    --                    A - D, moved
    type Amphipod = (AmphipodId, Bool) -- Can move
    data Cell = Entity Amphipod | Empty | Wall
    type Grid = Map Point Cell

    instance Eq AmphipodId where
        A == A = True
        B == B = True
        C == C = True
        D == D = True
        _ == _ = False

    instance Ord AmphipodId where
        A <= _ = True
        B <= A = False
        B <= _ = True
        C <= A = False
        C <= B = False
        C <= _ = True
        D <= D = True
        D <= _ = False

    instance Show AmphipodId where
        show A = "A"
        show B = "B"
        show C = "C"
        show D = "D"

    instance Eq Cell where
        Empty == Empty = True
        Wall == Wall   = True
        (Entity a) == (Entity b) = a == b
        _ == _ = False

    instance Ord Cell where
        Empty <= _ = True
        Wall <= Empty = False
        Wall <= _ = True
        (Entity a) <= (Entity b) = a < b
        (Entity _) <= _ = False

    instance Show Cell where
        show Empty = "Empty"
        show Wall = "Wall"
        show (Entity (id, active)) = printf "Entity {id = %s, active = %s}" (show id) (show active)


    parseCell :: Int -> Int -> Char -> Maybe (Point, Cell)
    parseCell x y '#' = Just ((x, y), Wall)
    parseCell x y '.' = Just ((x, y), Empty)
    parseCell x y ' ' = Nothing
    parseCell x y 'A' = Just ((x, y), Entity (A, True))
    parseCell x y 'B' = Just ((x, y), Entity (B, True))
    parseCell x y 'C' = Just ((x, y), Entity (C, True))
    parseCell x y 'D' = Just ((x, y), Entity (D, True))
    parseCell _ _  _  = error "Parse error"

    getEnergyPerStep' :: AmphipodId -> Int
    getEnergyPerStep' A = 1
    getEnergyPerStep' B = 10
    getEnergyPerStep' C = 100
    getEnergyPerStep' D = 1000


    getEnergyPerStep :: Cell -> Int
    getEnergyPerStep (Entity (A, _)) = 1
    getEnergyPerStep (Entity (B, _)) = 10
    getEnergyPerStep (Entity (C, _)) = 100
    getEnergyPerStep (Entity (D, _)) = 1000
    getEnergyPerStep _ = error "Not an amphipod, why is this even moving"

    isActiveAmphipod :: Cell -> Bool
    isActiveAmphipod (Entity (_, True)) = True
    isActiveAmphipod _ = False

    deactivate :: Cell -> Cell
    deactivate (Entity (id, True)) = Entity (id, False)
    deactivate c = c

    isAmphipod :: Cell -> Bool
    isAmphipod x = not (isEmpty x) && not (isWall x)

    isSpecific :: AmphipodId -> Cell -> Bool
    isSpecific i (Entity (j, _)) = i == j
    isSpecific _ _ = False

    isEmpty :: Cell -> Bool
    isEmpty = (== Empty)

    isWall :: Cell -> Bool
    isWall = (== Wall)

    add :: Point -> Point -> Point
    add (a, b) (c, d) = (a + c, b + d)

    neighbours :: [Point]
    neighbours = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    getDestinations :: AmphipodId -> [Point]
    getDestinations A = [(3,2), (3,3)]
    getDestinations B = [(5,2), (5,3)]
    getDestinations C = [(7,2), (7,3)]
    getDestinations D = [(9,2), (9,3)]

    bfs' :: Grid -> Set Point -> [(Int, Point)] -> [(Int, Point)]
    bfs' grid visited [] = []
    bfs' grid visited ((dist, start):rest) = (dist, start) : bfs' grid visited' rest'
        where nearby = map (add start) neighbours
              unvisited = filter (`notMember` visited) nearby
              next = filter (\x -> isEmpty (findWithDefault  Empty x grid)) unvisited
              visited' = foldl (flip Data.Set.insert) visited next
              rest' = rest ++ map (dist + 1,) next

    -- Scan the grid for the next points, and return a list of (distance, point), excluding the starting point
    bfs :: Grid -> Point -> [(Int, Point)]
    bfs grid start = filter ((/=0) . fst) $ bfs' grid Data.Set.empty [(0, start)]

    -- The points above the destinations only have 1 wall
    canStand :: Grid -> Point -> Bool
    canStand grid center = length (filter isWall $ map ((grid !) . add center) neighbours) >= 2

    canMove :: Grid -> (Point, Cell) -> Bool
    canMove grid (point, Entity (i, active))
      -- Overarching case, all are done
      | all (isSpecific i . (grid !)) (getDestinations i) = False
      -- More specific case, this amphipod is in the last destination
      | last (getDestinations i) == point = False
      | otherwise = True -- Active will be sorted out at a later stage
    canMove _ _ = False

    getPossibleAmphipods :: Grid -> [(Point, Cell)]
    getPossibleAmphipods grid = amphipods
        where amphipods = filter (canMove grid) $ Data.Map.toList grid

    getFirstDestination :: Grid -> AmphipodId -> [Point]
    getFirstDestination grid id = take 1 $ reverse $ filter (isEmpty . (grid ! )) points
        where points = getDestinations id

    getForbiddenDestinations :: AmphipodId -> [Point]
    getForbiddenDestinations id = concatMap getDestinations $ filter (/=id) [A, B, C, D]

    computeEnergy :: Int -> AmphipodId -> Int
    computeEnergy dist e = dist * getEnergyPerStep' e

    getNextGridForAmphipod :: Grid -> (Point, Cell) -> [(Int, Grid)]
    getNextGridForAmphipod grid (point, apod@(Entity (id, active))) = grids
        where points = filter (canStand grid . snd) $ bfs grid point
              filter' = if active then (`notElem` getForbiddenDestinations id) else (`elem` getFirstDestination grid id)
              points' = filter (filter' . snd) points
              grid' = Data.Map.insert point Empty grid
              apod' = deactivate apod
              grids = map (\(dist, p) -> (computeEnergy dist id, Data.Map.insert p apod' grid')) points'
    getNextGridForAmphipod _ _ = []

    getNextGrid :: Grid -> [(Int, Grid)]
    getNextGrid grid = concatMap (getNextGridForAmphipod grid) apods
        where apods = getPossibleAmphipods grid

    -- Check for all IDs if their destinations are covered by the same id
    isDone :: Grid -> Bool
    isDone grid = all (\id -> all (isSpecific id . (grid !)) (getDestinations id)) [A, B, C, D]

    printCell :: Maybe Cell -> Char
    printCell (Just Wall) = '#'
    printCell (Just Empty) = '.'
    printCell (Just (Entity (A, _))) = 'A'
    printCell (Just (Entity (B, _))) = 'B'
    printCell (Just (Entity (C, _))) = 'C'
    printCell (Just (Entity (D, _))) = 'D'
    printCell Nothing = ' '

    printGridPath :: Map Grid Grid -> Maybe Grid -> String
    -- printGridPath prev (Just grid) = (state ++ "\n\n") ++ printGridPath prev (Data.Map.lookup grid prev)
    --    where state = intercalate "\n" $ map (\y -> map (printCell . (`Data.Map.lookup` grid) . (, y)) [0..12]) [0..4]
    printGridPath _ _ = ""


    dijkstra' :: Set (Int, Grid) -> (Grid -> [(Int, Grid)]) -> (Grid -> Bool) -> Map Grid Int -> Map Grid Grid -> Int
    dijkstra' queue neighbours done distMap prev
      | done cur = trace (printGridPath prev (Just cur)) dist
      -- Ignore the node as it wasn't closer
      | dist > findWithDefault maxBound cur distMap = dijkstra' queue' neighbours done distMap prev
      | otherwise = dijkstra' queue'' neighbours done distMap' prev'
        where ((dist, cur), queue') = deleteFindMin queue
              next = trace (show dist) $ neighbours cur
              next' = map (\(w, g) -> (w + dist, g)) next
              next'' = filter (\(w, g) -> w <= findWithDefault maxBound g distMap) next'
              queue'' = foldl (flip Data.Set.insert) queue' next''
              distMap' = foldl (\p (w, g) -> Data.Map.insert g w p) distMap next''
              prev' = foldl (\p (_, g) -> Data.Map.insert g cur p) prev next''


    dijkstra :: Grid -> (Grid -> [(Int, Grid)]) -> (Grid -> Bool) -> Int
    dijkstra grid neighbours done = dijkstra' (Data.Set.singleton (0, grid)) neighbours done (Data.Map.singleton grid 0) Data.Map.empty

    solve = do
        d <- lines <$> getContents
        let grid = fromList $ catMaybes $ concat $ zipWith (\y row -> zipWith (`parseCell` y) [0..] row) [0..] d
        let val = dijkstra grid getNextGrid isDone
        printf "Hello, the energy required is %d" val
