YEAR=${YEAR:-$(cat .year)}

if [ -z "$AOC_SESSION" ] && [ -f .session ] ; then
  source .session
fi

if [ -z "$AOC_SESSION" ]; then
  echo "Please set AOC_SESSION to your session cookie, otherwise I can't get your inputs"
  exit 1
fi

if [ -f "$YEAR/prepare.sh" ]; then
  cd "$YEAR"
  ./prepare.sh $1
  cd ..
fi

file="$YEAR/inputs/$(printf "%02d" $1).txt"
mkdir -p $(dirname $file)
curl -o $file https://adventofcode.com/${YEAR}/day/$1/input --cookie "session=$AOC_SESSION"
