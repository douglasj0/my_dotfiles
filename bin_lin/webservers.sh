#!/bin/bash

if [ -n "${XDG_CURRENT_DESKTOP}" ]; then
  case "${XDG_CURRENT_DESKTOP}" in
    GNOME)
      TERMINAL="gnome-terminal"
    ;;
    XFCE)
      TERMINAL="xfce4-terminal"
    ;;
    #KDE)
    #;;
    #LXDE)
    #;;
  esac
fi

${TERMINAL} --tab -e "ssh web1" --tab -e "ssh web2" --tab -e "ssh web3" \
            --tab -e "ssh web4" --tab -e "ssh web5" --tab -e "ssh web6" \
            --tab -e "ssh web7"
