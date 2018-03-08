#!/bin/bash
# Copyright (C) 2007 Daniel William Maddler <maddler@maemopeople.org>
# All rights reserved.
#
# This program is a free software; you can redistribute it
# and/or modify it under the terms of the GNU General Public
# License (version 2) as published by the FSF - Free Software
# Foundation (http://www.gnu.org)
#
# https://www.fsf.org/licensing/licenses/gpl.html

# BASIC CONFIGURATION

REMOTE_USER="maddler"
REMOTE_ADDRESS="192.168.1.199"
VPN_IP_LOCAL="10.0.0.1"
VPN_IP_REMOTE="10.0.0.2"

# NOTHING BELOW THIS POINT SHOULD NEED TO BE CHANGED *I HOPE*

LOCAL_GW=`route -n | grep "^0.0.0.0" | gawk {'print $2'}`
sudo /usr/sbin/pppd updetach noauth silent nodeflate pty "/usr/bin/ssh ${REMOTE_USER}@${REMOTE_ADDRESS} sudo /usr/sbin/pppd nodetach notty noauth" ipparam vpn ${VPN_IP_LOCAL}:${VPN_IP_REMOTE}

# SETTING NEW ROUTES

sudo route add -host $REMOTE_ADDRESS gw $LOCAL_GW
sudo route del -net 0.0.0.0/0 gw $LOCAL_GW
sudo route add default gw $VPN_IP_REMOTE
