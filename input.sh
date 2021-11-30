mkdir inputs/
curl -o "src/Day$(printf %02d $1)/input.txt" https://adventofcode.com/2021/day/$1/input --cookie "session=$AOC_SESSION"
