#!/bin/bash

rsync -avz --delete /Users/djackson/Documents/Grubhub/ /Users/djackson/Dropbox/Documents --dry-run

read -p "^^^ This is what rsync would do, continue? (YN)" yn
case $yn in
  [Yy]*) rsync -avz --delete /Users/djackson/Documents/Grubhub/ /Users/djackson/Dropbox/Documents 
  ;;
  [Nn]*) echo Aborting
         exit 0
  ;;
  *) echo "Please answer Yes|No";;
esac
