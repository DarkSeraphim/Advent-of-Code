if [ ! -z "$DEBUG" ]; then
  stack build --trace --profile --executable-profiling --library-profiling
  stack run --trace --profile aoc-prebuild
  stack run --trace --profile -- +RTS -xc -RTS $1 $2
else
  echo "Building..."
  stack run aoc-prebuild > /dev/null 2>&1
  echo "Running..."
  stack run $1 $2 2> /dev/null
fi
