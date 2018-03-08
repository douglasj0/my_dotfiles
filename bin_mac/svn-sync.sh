#!/bin/sh

SVN=/usr/bin/svn

for i in \
  ${HOME}/.mutt \
  ${HOME}/.common \
  ${HOME}/.common/Things \
  ${HOME}/bin \
  ${HOME}/examples
do
  echo -=-=-=-
  echo Checking $i
  $SVN status $i
  echo Getting updates
  $SVN update $i
done
