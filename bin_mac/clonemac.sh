#!/bin/sh
#
# clone - Copy Mac OS X from one location to another
#

if [ `whoami` != "root" ]; then
 echo "This script requires root privileges."
 return 1
fi

#
# Grab the arguments or bug out
#

NAME=`basename $0`

if [ $# == 1 ]; then
 SRC="/"
 DEST="$1"
else
 if [ $# == 2 ]; then
  SRC="$1"
  DEST="$2"
 else
  echo "Usage: $NAME [source] destination"
  return 0
 fi
fi

#
# Strip any trailing slashes
#
SRC=`echo "$SRC" | sed 's/\/+$//'`
DEST=`echo "$DEST" | sed 's/\/+$//'`

#
# Make sure the source and destination exist
#
if [ ! -d "$SRC" ]; then
 echo "The source \"$SRC\" doesn't exist!"
 return 1
fi

if [ ! -d "$DEST" ]; then
 echo "The destination \"$DEST\" doesn't exist!"
 return 1
fi

#
# If SRC and DEST were the same it would be bad
#
if [ "$DEST" == "$SRC"  ]; then
 echo "The source and destination must be different!"
 return 1
fi

#
# For each item that exists... Clone it!
#
for X in Users Applications Developer Library System \
 sw private usr bin sbin mach_kernel mach.sym \
 .hidden .VolumeIcon.icns Desktop\ Folder
do
 if [ -e "${SRC}/$X" ]; then
  echo "Copying $X ..."
  chflags -R nouchg,noschg "${SRC}/$X"
  ditto -rsrc "${SRC}/$X" "${DEST}/$X"
 fi
done

cd "$DEST"

#
# Create necessary symbolic links
#
echo "Creating symbolic links..."
for X in etc var cores tmp; do
 if [ ! -e "$X" ]; then
  ln -s "private/$X" "$X"
 fi
done

#
# Don't forget the mach->mach.sym link
#
if [ ! -e "mach" ]; then
 ln -s mach.sym mach
fi

#
# Create required directories
#
echo "Creating required directories..."
for X in dev Volumes Network; do
 if [ ! -e "$X" ]; then
  mkdir $X
 fi
done

echo "The Clone Was Successful!"
