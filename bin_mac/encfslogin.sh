#!/bin/bash
 
ENCFS="/usr/local/bin/encfs" 
#ENCDIR="$HOME/Dropbox/.encrypted"
ENCDIR="$HOME/Dropbox/.private"
#DECDIR="$HOME/Documents/grubhub"
DECDIR="$HOME/Private"
 
#security find-generic-password -ga encfs 2>&1 >/dev/null | cut -d'"' -f2 | "$ENCFS" -o "volname=grubhub" -S "$ENCDIR" "$DECDIR"
security find-generic-password -ga encfs 2>&1 >/dev/null | cut -d'"' -f2 | "$ENCFS" -o "volname=Private" -S "$ENCDIR" "$DECDIR"
