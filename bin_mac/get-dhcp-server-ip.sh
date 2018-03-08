#!/bin/sh
echo "en0 DHCP -- Ethernet"
ipconfig getoption en0 server_identifier
echo " "
echo "en1 DHCP -- Wireless"
ipconfig getoption en1 server_identifier
