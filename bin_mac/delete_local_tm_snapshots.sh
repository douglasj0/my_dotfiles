#!/bin/bash

# Cleanup
rm -f /tmp/snapshots.txt /tmp/snapshot_dates.txt

# generage snapshot list
tmutil listlocalsnapshots / > /tmp/snapshots.txt

awk -F. '/com.apple/ {print $4}' /tmp/snapshots.txt > /tmp/snapshot_dates.txt

while read DATES; do
  echo "Deleting ${DATES}"
  sudo tmutil deletelocalsnapshots ${DATES}
done < /tmp/snapshot_dates.txt
