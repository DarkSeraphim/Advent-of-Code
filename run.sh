YEAR=${YEAR:-$(cat .year)}
cat ./$YEAR/inputs/$(printf "%02d" $1).txt | ./manual.sh $1 $2
