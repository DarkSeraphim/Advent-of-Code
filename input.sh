YEAR=${YEAR:-$(cat .year)}

file="$YEAR/inputs/input.txt"
mkdir -p $(dirname $file)
curl -o $file https://adventofcode.com/${YEAR}/day/$1/input --cookie "session=$AOC_SESSION"
