#!/bin/bash -e

day=$(date -v +1d +%-d)
dir="day${day}"

# create the directory for the day
echo "Creating $dir/..."
mkdir -p $dir

# touch the real and test data in there
if [ ! -e "$dir/test.txt" ]; then
    touch "$dir/test.txt"
fi

if [ ! -e "$dir/real.txt" ]; then
    touch "$dir/real.txt"
fi

# write the core day script
if [ ! -e "$dir/day${day}.lisp" ]; then
    cp ./base.lisp "$dir/day${day}.lisp"
fi
