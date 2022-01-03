{-# LANGUAGE NamedFieldPuns #-}
module Day18.Part2 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (GenParser, char, (<|>), parse, eof, endBy1)
    import Helpers.Parsec (number)
    import Text.Parsec.Char (endOfLine)
    import Helpers.Input (orFail)

    import Debug.Trace (trace)

    data Node = Intermediate { left :: Node, right :: Node} | Leaf Int deriving Eq

    parseLines :: GenParser Char st [Node]
    parseLines = parseNode `endBy1` endOfLine  <* eof

    parseNode :: GenParser Char st Node
    parseNode = parseIntermediate <|> parseValue

    parseIntermediate :: GenParser Char st Node
    parseIntermediate = Intermediate <$> (char '[' *> parseNode <* char ',') <*> (parseNode <* char ']')

    parseValue :: GenParser Char st Node
    parseValue = Leaf <$> number

    leafValue (Leaf value) = value
    leafValue _ = error "Not a leaf"

    trySplit :: Node -> Maybe Node
    trySplit (Leaf value)
      | value >= 10 = Just Intermediate {left = Leaf left, right = Leaf right}
      | otherwise   = Nothing
        where left = value `div` 2
              right = value - left
    trySplit (Intermediate left right) =
        case leftSplit of
            Just newLeft -> Just Intermediate {left = newLeft, right}
            Nothing ->
                case rightSplit of
                    Just newRight -> Just Intermediate {left, right = newRight }
                    Nothing -> Nothing

      where leftSplit = trySplit left
            rightSplit = trySplit right

    data Bubble = L | R

    bubbleDown :: Int -> Bubble -> Node -> Node
    bubbleDown add _ (Leaf value) = Leaf (value + add)
    bubbleDown add bubble (Intermediate left right) =
      case bubble of
        L -> Intermediate {left = bubbleDown add bubble left, right}
        R -> Intermediate {left, right = bubbleDown add bubble right}

    tryExplode' :: Int -> Node -> Maybe (Node, Maybe Int, Maybe Int)
    tryExplode' 4 Intermediate {left, right} = Just (Leaf 0, Just (leafValue left), Just (leafValue right))
    --tryExplode' 4 (Leaf v) = error (printf "Exploded on leaf %d" v)
    tryExplode' _ (Leaf _) = Nothing
    tryExplode' depth (Intermediate left right)
      -- For depth == 3 I know that a Just will be a new explosion
      -- For less depth (0-2) it's a bubble up, one side will be Nothing
      | depth == 3 = case leftExplode of
          -- Left splits should always have a right value at this depth
          Just (newLeft, leftVal, Just rightVal) ->
            -- Consume right value, propagate leftVal up
            Just (Intermediate { left = newLeft, right = bubbleDown rightVal L right }, leftVal, Nothing)
          Nothing ->
            case rightExplode of
              Just (newRight, Just leftVal, rightVal) ->
                Just (Intermediate { left = bubbleDown leftVal R left, right = newRight }, Nothing, rightVal)
              Nothing -> Nothing
              _ -> error "Unreachable"
          _ -> error "Unreachable"
      | otherwise = case leftExplode of
          -- We found the bubble
          Just (newLeft, Nothing, Just rightVal) -> Just (Intermediate { left = newLeft, right = bubbleDown rightVal L right}, Nothing, Nothing)
          -- Try again with root
          Just (newLeft, leftVal, rightVal) -> Just (Intermediate { left = newLeft, right }, leftVal, rightVal)
          Nothing ->
            case rightExplode of
              -- We found the bubble
              Just (newRight, Just leftVal, Nothing) -> Just (Intermediate { left = bubbleDown leftVal R left, right = newRight}, Nothing, Nothing)
              -- Try again with root
              Just (newRight, leftVal, rightVal) -> Just (Intermediate { left, right = newRight}, leftVal, rightVal)
              Nothing -> Nothing
      where leftExplode = tryExplode' (depth + 1) left
            rightExplode = tryExplode' (depth + 1) right

    fst3 :: (a, b, c) -> a
    fst3 (a, b, c) = a

    tryExplode :: Node -> Maybe Node
    tryExplode node = fst3 <$> tryExplode' 0 node

    -- I lack names, so this will try to split
    reduce' :: Node -> Node
    reduce' node = maybe node reduce $ trySplit node

    -- And this is for explodes
    reduce :: Node -> Node
    reduce node = case reducedNode of

        -- If tryExplode was successful, attempt it again
        Just node' -> reduce node'
        -- Otherwise fall back to a split
        Nothing -> reduce' node
      where reducedNode = tryExplode node

    toString :: Node -> String
    toString Intermediate {left, right} = printf "[%s,%s]" (toString left) (toString right)
    toString (Leaf value) = printf "%d" value

    snailAdd :: Node -> Node -> Node
    snailAdd left right = reduce Intermediate { left, right }

    magnitude :: Node -> Int
    magnitude Intermediate {left, right} = 3 * magnitude left + 2 * magnitude right
    magnitude (Leaf value) = value

    findMagnitude (a, b)
     | a /= b    = magnitude $ snailAdd a b
     | otherwise = -1

    solve = do
        snailNumbers <-  orFail . parse parseLines "Line" =<< getContents
        let pairs = concatMap (zip snailNumbers . repeat) snailNumbers
        let sum = maximum $ map findMagnitude pairs
        printf "The highest magnitude for any pair is %d\n" sum
