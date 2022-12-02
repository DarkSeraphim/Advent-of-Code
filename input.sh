YEAR=${YEAR:-$(cat .year)}

if [ -f "$YEAR/prepare.sh" ]; then
  cd "$YEAR"
  ./prepare.sh $1
  cd ..
fi

file="$YEAR/inputs/$(printf "%02d" $1).txt"
mkdir -p $(dirname $file)
curl -o $file https://adventofcode.com/${YEAR}/day/$1/input --cookie "session=$AOC_SESSION"
