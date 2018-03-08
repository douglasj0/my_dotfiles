#!/bin/bash
 
ENCFS="/usr/local/bin/encfs" 
ENCDIR="$HOME/Dropbox/.encrypted"
DECDIR="$HOME/Documents/grubhub"
 
security find-generic-password -ga encfs 2>&1 >/dev/null | cut -d'"' -f2 | "$ENCFS" -o "volname=grubhub" -S "$ENCDIR" "$DECDIR"
