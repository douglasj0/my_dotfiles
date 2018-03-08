#!/bin/sh
perl -p -i -e 'BEGIN { print "Converting UNIX to DOS.\n" ; } END { print "Done.\n" ; } s/(?<!\r)\n$/\r\n/' $*
