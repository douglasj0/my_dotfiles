#!/bin/bash
#
#
#date "+%I %M %p. get to work" |say -v Victoria
#
#min=`date +%M`
#hour=`date +%H`
#
hour=`date | awk {'print $4'} | cut -d":" -f1`
min=`date | awk {'print $4'} | cut -d":" -f2`

if [ $hour -gt 12 ]; then
    say -v Fred `expr $hour - 12`
else
    say -v Victoria $hour
fi
say -v Victoria $min
say -v Victoria "time to go to work"
exit
