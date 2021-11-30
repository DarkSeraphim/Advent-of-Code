file="src/Day$(printf %02d $1)/input.txt"
mkdir -p $(dirname $file)
curl -o $file https://adventofcode.com/2020/day/$1/input --cookie "session=$AOC_SESSION"
