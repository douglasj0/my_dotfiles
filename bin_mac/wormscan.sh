#!/bin/sh
#
#sudo tcpdump -i en1 -l -n arp | grep 'arp who-has' | head -100 | awk '{ print $NF }' | sort | uniq -c | sort -n

sudo tcpdump -i en1 -l -n arp
