#!/bin/sh
if [[ -z $1 ]]; then
  # robg note: Please enter the next two lines as one without
  # any spaces between the "/" and the "R"
  /System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend
else
  USERID=`id -u $1`;
  if [[ -z $USERID ]]; then
    exit -1;
  fi;
  # robg note: Please enter the next two lines as one without
  # any spaces between the "/" and the "R"
  /System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -switchToUserID $USERID
fi;
