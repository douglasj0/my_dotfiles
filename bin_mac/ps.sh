#!/bin/bash
#echo hi
pformat="user=UID,pid,ppid,cpu=C,start=STIME,tty,time,command"
psarglist=
psarglistlong=
args="`echo \"$*\" | /usr/bin/sed -e 's/-ef$//' -e 's/-ef/-/'`"
set -- $args
#echo "$args"
#set $args
while true ; do
	if [ -z "$1" ]; then
		break;
	fi
	optchar=${1:1}
	case $optchar in
		O|o|p|t|U)
			shift
			psarglistlong="$psarglistlong -$optchar $1"
		;;
		*)
			psarglist="$psarglist$optchar"
		;;
	esac
	shift
done
/bin/ps -ax$psarglist $psarglistlong -o $pformat
