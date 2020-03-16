#! /usr/bin/env bash

cd '/Volumes/SSD/hockey/data/xlsx'
csv_path='/Volumes/SSD/hockey/data/csv'

for dir in *; do
    for file in "$dir"/*.xlsx; do
        ssconvert "$file" "$csv_path/${file%.xlsx}.csv"
    done
done
