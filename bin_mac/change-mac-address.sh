#!/bin/bash
# Created from http://www.online-tech-tips.com/computer-tips/how-to-change-mac-address/

ORIG_MAC=$(ifconfig en0 | grep ether | awk '{print $2}')
SAVED_MAC="/tmp/original-mac.txt"
NEW_MAC=$(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')

case "$1" in
  replace) #replace mac with new one and save orig
    if [[ "$(ifconfig en0 | grep ether | awk '{print $2}')" = "$(cat ${SAVED_MAC})" ]]; then
      echo ${ORIG_MAC} > ${SAVED_MAC}

      echo "Replacing original MAC address with ${NEW_MAC}"
      sudo ifconfig en0 ether ${NEW_MAC}
      sudo ifconfig en0 down
      sleep 2
      sudo ifconfig en0 up

      echo "MAC was ${ORIG_MAC}, now changed to: $(ifconfig en0 | grep ether | awk '{print $2}')"
    else
      echo "MAC address has already been replaced, exiting"
      exit 1
    fi
  ;;
  restore)
    ORIG_MAC=$(cat ${SAVED_MAC}) 

    echo "Restoring original MAC address"
    sudo ifconfig en0 ether ${ORIG_MAC}
    sudo ifconfig en0 down 
    sleep 2
    sudo ifconfig en0 up

    echo "MAC restored to: $(ifconfig en0 | grep ether | awk '{print $2}')"
  ;;
  *)
    echo "Usage:  $0 (replace|restore)"
    echo "  ex.   $0 replace"
    echo
    echo "  replace - replace MAC with random string, save current value to /tmp"
    echo "  restore - restore original MAC address to saved value"
    echo
    exit 0
  ;;
esac
