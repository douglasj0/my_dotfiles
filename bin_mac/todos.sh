#!/bin/sh
perl -p -i -e 's/(?<!\r)\n$/\r\n/' $*
