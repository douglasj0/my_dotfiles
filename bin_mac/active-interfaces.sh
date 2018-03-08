#!/bin/sh

for iface in  `/sbin/ifconfig -lu` ; do
    case $iface in
    vbox* | lo*)     continue ;;
    esac
    /sbin/ifconfig $iface | /usr/bin/grep -q 'inet ' && echo $iface
done
