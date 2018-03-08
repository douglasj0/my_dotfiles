#!/bin/sh
if [ ! $1 ]; then
        echo "What man page do you want?";
        exit;
fi;

if [ ! $2 ]; then
        MAN_PAGE=$1;
        MAN_SECTION="1";
else
        MAN_PAGE=$2;
        MAN_SECTION=$1;
fi;

CACHE_PATH="/tmp/ManCache"
CACHE_FILENAME=$CACHE_PATH/$MAN_PAGE.$MAN_SECTION.pdf

if [[ ! -d "$CACHE_PATH" ]]; then
        mkdir -p $CACHE_PATH;
fi;

if [[ -r $CACHE_FILENAME ]]; then
        open $CACHE_FILENAME;
elif [[ `man -w "$@"` != "" ]]; then
        man -t $@ | pstopdf -i -o $CACHE_FILENAME; open $CACHE_FILENAME;
fi;
