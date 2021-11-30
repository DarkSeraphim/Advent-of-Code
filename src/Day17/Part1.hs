module Day17.Part1 (solve) where
  import Data.List (intercalate)
  solve = do
    ls <- lines <$> getContents
    putStrLn $ intercalate "\n" ls
