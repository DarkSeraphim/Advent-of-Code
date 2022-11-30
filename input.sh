YEAR=${YEAR:-$(cat .year)}

if [ -f "$YEAR/prepare.sh" ]; then
  "$YEAR/prepare.sh" $1
fi

file="$YEAR/inputs/$(printf "%02d" $1).txt"
mkdir -p $(dirname $file)
curl -o $file https://adventofcode.com/${YEAR}/day/$1/input --cookie "session=$AOC_SESSION"
