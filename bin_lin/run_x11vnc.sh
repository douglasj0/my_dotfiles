#!/bin/bash
#x11vnc -usepw
#
# Control x11vnc, start it persistaly, stop it, check for running
#
# 20130123 - djackson

case "$1" in
start)
    echo "Starting x11vnc"
    /usr/bin/x11vnc -many -display :0 >/dev/null 2>&1 &
    echo $?
    ;;
stop)
    echo "Shutting down x11vnc"
    /usr/bin/x11vnc -R stop -display :0 >/dev/null 2>&1
    echo $?
    ;;
status)
    echo "Checking for x11vnc process"
    ps -ef | grep x11vnc | egrep -v "grep|status"
    echo $?
    ;;
*)
    echo "Usage: run_x11vnc.sh [start|stop|status]" >&2
    exit 3
    ;;
esac
