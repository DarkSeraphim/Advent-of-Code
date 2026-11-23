module Day16.Part2 (solve) where
import Text.Printf (printf)
import Data.Map (Map, fromList, keys, (!))
import Helpers.Parsec (Parser, number, sepBy1', endBy1', parseInput)
import Text.Parsec (many1, noneOf, string, endOfLine)

type Properties = Map String Int

propertyP :: Parser (String, Int)
propertyP = (,) <$> many1 (noneOf ": \r\n") <*> (string ": " *> number)

propertiesP :: Parser Properties
propertiesP = fromList <$> propertyP `sepBy1'` string ", "

sueP :: Parser (Int, Properties)
sueP = (,) <$> (string "Sue " *> number <* string ": ") <*> propertiesP

suesP :: Parser [(Int, Properties)]
suesP = sueP `endBy1'` endOfLine

theRealSue :: Properties
theRealSue = fromList [
    ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)
  ]

matchLeft :: Properties -> Properties -> Bool
matchLeft l r = all match (keys l)
  -- We know that r contains all possible keys l can have
    where match k = compareFor k l' r'
            where l' = l ! k
                  r' = r ! k
          compareFor "cats" l' r' = l' > r'
          compareFor "trees" l' r' = l' > r'
          compareFor "pomeranians" l' r' = l' < r'
          compareFor "goldfish" l' r' = l' < r'
          compareFor _ l' r' =  l' == r'

solve :: IO ()
solve = do
  sues <- parseInput suesP

  printf "Matching Sues: %s\n" (show $ map fst (filter ((`matchLeft` theRealSue) . snd) sues))
