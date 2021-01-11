#!/bin/bash

# Cleanup
rm snapshots.txt snapshot_dates.txt

# generage snapshot list
tmutil listlocalsnapshots / > snapshots.txt

awk -F. '/com.apple/ {print $4}' snapshots.txt > snapshot_dates.txt

while read DATES; do
  echo "Deleting ${DATES}"
  sudo tmutil deletelocalsnapshots ${DATES}
done < snapshot_dates.txt
