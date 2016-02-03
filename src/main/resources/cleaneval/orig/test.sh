#!/bin/bash

while read p; do
  grep "$p" *.html.utf8 -B 2 -A 2
  echo "//////"
  echo "//////"
done </Users/thijs/Desktop/nonumber.txt