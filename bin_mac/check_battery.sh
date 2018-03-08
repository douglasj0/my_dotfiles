#!/bin/sh
#
#Linux:
#cat /proc/acpi/battery/BAT*/info | grep -A 1 "design capacity"design capacity: 4500 mAh
#
# Original Capacity s/b 4400 mAh
# check the number after capacity
#
#last full capacity: 4282 mAh
ioreg -l | grep -i IOBatteryInfo | awk -F\" '{ print $4 $5 " of 4400" }'

