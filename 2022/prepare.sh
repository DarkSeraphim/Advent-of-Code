day=$(printf "%02d" $1)

mkdir -p src/Day$day

cat <<eof > src/Day$day/Part1.hs
module Day$day.Part1 (solve) where
import Text.Printf (printf)

solve = do
  _ <- getContents
  printf "Day $1 part 1 has not been implemented yet" 
eof

cat <<eof > src/Day$day/Part2.hs
module Day$day.Part2 (solve) where
import Text.Printf (printf)
solve = do
  _ <- getContents
  printf "Day $1 part 2 has not been implemented yet" 
eof

