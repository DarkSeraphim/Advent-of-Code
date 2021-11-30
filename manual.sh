if [ ! -z "$DEBUG" ]; then
  stack run aoc-prebuild
  stack run $1 $2
else
  echo "Building..."
  stack run aoc-prebuild > /dev/null 2>&1
  echo "Running..."
  stack run $1 $2 2> /dev/null
fi
