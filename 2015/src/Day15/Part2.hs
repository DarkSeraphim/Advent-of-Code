module Day15.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, sepBy1', endBy1', parseInput)
import Text.Parsec ( space, string, endOfLine, noneOf, many1 )
import Data.Map (Map, fromList, delete, elems, (!))
import Data.List (transpose)

type Properties = Map String Int

propertyP :: Parser (String, Int)
propertyP = (,) <$> many1 (noneOf " \r\n") <*> (space *> number)

propertiesP :: Parser Properties
propertiesP = fromList <$> propertyP `sepBy1'` string ", "

ingredientP :: Parser (String, Properties)
ingredientP = (,) <$> (many1 (noneOf ":\r\n") <* string ": ") <*> propertiesP

ingredientsP :: Parser [(String, Properties)]
ingredientsP = ingredientP `endBy1'` endOfLine

generateCounts :: [a] -> Int -> [[Int]]
generateCounts [] _ = [[]]
generateCounts [_] n = [[n]]
generateCounts (_:r) n = concatMap (\n' -> map (n' :) $ generateCounts r (n - n')) [1..avail]
  where avail = n - length r

removeCalories :: Properties -> Properties
removeCalories = delete "calories"

scoreIngredient :: Properties -> Int -> [Int]
scoreIngredient props n = map (n * ) (elems props)

scoreCookie :: [Properties] -> [Int] -> [Int] -> Int
scoreCookie props calories ns
  | caloryScore /= 500 = 0 
  | any (<= 0) propScores = 0
  | otherwise = product propScores
    where m = zipWith scoreIngredient props ns
          m' = transpose m
          propScores = map sum m'
          caloryScore = sum $ zipWith (*) calories ns

solve :: IO ()
solve = do
  ingredients <- parseInput ingredientsP
  printf "ingredients %s\n" (show ingredients)
  let properties = map (removeCalories . snd) ingredients
      caloryCounts = map ((! "calories") . snd) ingredients
      counts = generateCounts properties 100
      res = maximum $ map (scoreCookie properties caloryCounts) counts
  printf "Highest total score is %d\n" res
