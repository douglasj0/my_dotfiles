#!/bin/sh

GIT=/opt/git/bin/git

for i in ${HOME}/common
do
  echo -=-=-=-
  cd $i
  echo Checking $i
  $GIT status
  echo Pulling from remote to $i
  $GIT pull
  echo Pushing to remote to $i
  $GIT push
done
