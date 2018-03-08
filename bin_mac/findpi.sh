#!/bin/bash
#
# Note:  MAC address is only findable on the local subnet

BROADCAST=$(ifconfig en0 | awk '/broadcast/ {print $NF}')

if [ ${BROADCAST} == "192.168.2.255" ]; then
   # Home network
   IPRANGE="192.168.2.0/24"
   echo IPRANGE = $IPRANGE
else
   # CHI1 Office?
   IPRANGE="10.1.138.0/23"
   echo IPRANGE = $IPRANGE
fi

sudo nmap -sP ${IPRANGE} | awk '/^Nmap/{ip=$NF}/B8:27:EB/{print ip}' | sed -e 's/^(//' -e 's/)$//'
