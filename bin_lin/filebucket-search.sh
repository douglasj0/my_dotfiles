#!/bin/bash
# From Instant Puppet 3 Starter, 2013
# By Jo Rhett
#
FILE=$1
FOUND=0
CLIENTBUCKET=$(puppet config print clientbucketdir)

echo "Searching for local backups of $FILE..."
for backup in $(find $CLIENTBUCKET -type f -name paths \
  -exec grep -l $FILE {} \; |xargs -r ls -t);
do
  hash=$( basename $(dirname $backup))
  filename=$(< $backup )
  modify_time=$(stat --format '%y' $backup)
  echo -e "$filename\t$hash\t$modify_time"
  FOUND=$((FOUND+1))
done

if [ $FOUND -gt 0 ]; then
  if [ $FOUND -eq 1 ]; then
	echo "1 backup was found."
  elif [ $FOUND -gt 1 ]; then
	echo "$FOUND backups were found."
  fi
   echo ""
   echo "To view a file: puppet filebucket get -b $CLIENTBUCKET <hash>"
   echo "To restore a file: puppet filebucket restore -b $CLIENTBUCKET /new/path <hash>"

else
    echo "No previous versions of the $FILE were found."
fi
