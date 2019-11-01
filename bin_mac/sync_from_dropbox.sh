#!/bin/bash

rsync -avz --delete /Users/djackson/Dropbox/Documents/ /Users/djackson/Documents/GrubHub  --dry-run

read -p "^^^ This is what rsync would do, continue? (YN)" yn
case $yn in
  [Yy]*) echo rsync -avz --delete /Users/djackson/Dropbox/Documents/ /Users/djackson/Documents/GrubHub
  ;;
  [Nn]*) echo Aborting
         exit 0
  ;;
  *) echo "Please answer Yes|No";;
esac
