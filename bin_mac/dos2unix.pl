#!/bin/sh
perl -p -i -e 'BEGIN { print "Converting DOS to UNIX.\n" ; } END { print "Done.\
n" ; } s/\r\n$/\n/' $*
