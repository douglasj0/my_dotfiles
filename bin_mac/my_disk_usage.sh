#!/bin/sh
#
find . -user djackson -type f -exec du -k {} \; | awk '{ s=s+$1 } END { print "Total used: ",s }' 
