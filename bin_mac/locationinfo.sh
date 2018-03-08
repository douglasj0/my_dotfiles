#!/bin/sh

# display location info
# Geoffrey M. Ghose

ARD=/System/Library/CoreServices/RemoteManagement/ARDAgent.app/Contents/Support/networksetup-panther
GATEWAY=`route get default 2> /dev/null | grep gateway | awk '{print $2}'`
SSID=`/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I\
| grep ' SSID:' | cut -d ':' -f 2 | tr -d ' '`
BSSID=`/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I\
| grep ' BSSID:' | cut -d ':' -f 2- | tr -d ' '`
INTF=`route get default | grep interface | awk '{print $2}'`
IP=`ifconfig $INTF | grep 'inet ' | cut -d ' ' -f 2`
if [ $GATEWAY ]; then
GATEWAYMAC=`arp $GATEWAY | awk '{print $4}'`
fi
LOCATION=`scselect 2>&1 | grep '*' - | tail -n 1 | awk '{print $3}' | tr -d "()"`
HOSTNAME=`hostname`
SERVICE_GUID=`echo "open|||get State:/Network/Global/IPv4|||d.show" | \
tr '|||' '\n' | scutil | grep "PrimaryService" | awk '{print $3}'`
INTERF=`echo "open|||get Setup:/Network/Service/$SERVICE_GUID|||d.show" |\
tr '|||' '\n' | scutil | grep "UserDefinedName" | awk -F': ' '{print $2}'`

echo Location= $LOCATION
if [ $GATEWAYMAC ]; then
echo Gateway MAC= $GATEWAYMAC
fi
if [ $BSSID ]; then
echo BSSID= $BSSID
fi
if [ $SSID ]; then
echo SSID= $SSID
fi
echo IP= $IP
echo Hostname= $HOSTNAME
echo Interface= $INTERF
function getproxy {
GETPROXY="-get${1}proxy"
HOST=`$ARD $GETPROXY "$INTERF" 2> /dev/null | grep Server | awk '{print $2}'`
if [ $HOST ]; then
PORT=`$ARD $GETPROXY "$INTERF" 2> /dev/null | grep Port | awk '{print $2}'`
STATUS=`echo " " | telnet $HOST $PORT 2> /dev/null`
MATCH=`echo $STATUS | grep Connected - | tr -d '\n' | tr -d ' '` 
if [ $MATCH ]; then
echo " " $1: $HOST $PORT Reachable
if [ $1 == "web" ]; then
WEBPROXY="-x $HOST:$PORT"
fi
else
echo " " $1: $HOST $PORT Not Reachable
fi
fi
}
echo Set Proxies:
getproxy ftp
getproxy web
getproxy secureweb
getproxy streaming
getproxy gopher
getproxy socksfirewall

GEO=`curl -m 3 $WEBPROXY -s http://api.hostip.info/rough.php | grep City - | sed -e 's/^City: //'`
echo Where: $GEO
