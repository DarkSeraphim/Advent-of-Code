if [ ! -z "$DEBUG" ]; then
  stack build --trace --profile --executable-profiling --library-profiling --no-executable-stripping
  stack run --no-executable-stripping --trace --profile aoc-prebuild
  stack run --no-executable-stripping --trace --profile -- +RTS -xc -RTS $1 $2
else
  echo "Building..."
  stack run aoc-prebuild --no-executable-stripping > /dev/null 2>&1
  echo "Running..."
  stack run --no-executable-stripping $1 $2 2> /dev/null
fi
