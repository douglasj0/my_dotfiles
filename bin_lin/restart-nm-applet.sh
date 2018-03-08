#!/bin/bash
#
# NM indicator does not show VPN submenu or Wireless Networks on login
# https://bugs.launchpad.net/ubuntu/+source/network-manager/+bug/983583

killall nm-applet ; nm-applet >/dev/null 2>/dev/null &
