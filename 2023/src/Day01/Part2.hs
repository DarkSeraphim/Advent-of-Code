module Day01.Part2 (solve) where
import Text.Printf (printf)
import Data.Char (isDigit)

parse :: String -> String
parse [] = []
parse ('o':'n':'e':x) = '1' : parse ('e':x)
parse ('t':'w':'o':x) = '2' : parse ('o':x)
parse ('t':'h':'r':'e':'e':x) = '3' : parse ('e':x)
parse ('f':'o':'u':'r':x) = '4' : parse x
parse ('f':'i':'v':'e':x) = '5' : parse ('e':x)
parse ('s':'i':'x':x) = '6' : parse x
parse ('s':'e':'v':'e':'n':x) = '7' : parse ('n':x)
parse ('e':'i':'g':'h':'t':x) = '8' : parse ('t':x)
parse ('n':'i':'n':'e':x) = '9' : parse ('e':x)
--parse ('z':'e':'r':'o':x) = '0' : parse x
parse (d:x)
  | isDigit d = d : parse x
  | otherwise  = parse x

combineNumber :: [a] -> [a]
combineNumber l = [head l, last l]

solve :: IO ()
solve = do
  lines' <- lines <$> getContents
  let lines'' = map parse lines'
  let numbers = map ((read :: String -> Int) . combineNumber) lines''

  printf "Calibration value is %d\n" (sum numbers)

