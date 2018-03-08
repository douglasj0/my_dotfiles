#!/bin/sh
# convert mac line endings to unix ones
while [ $# -gt "0" ]
do
sed 's/^M/\/g' "$1" > "$1.unix"
shift
done
