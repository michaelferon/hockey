#! /usr/bin/env bash

cd '/Volumes/SSD/hockey/data/csv'

for dir in *; do
    [ -f "../clean/${dir}.csv" ] && rm "../clean/${dir}.csv"
    touch "../clean/${dir}.csv"
    first="$(ls $dir | head -n 1)"
    cat "$dir/$first" | head -n 1 > "../clean/${dir}.csv"

    for file in "$dir"/*; do
        cat "$file" | tail -n +2 >> "../clean/${dir}.csv"
    done
done
