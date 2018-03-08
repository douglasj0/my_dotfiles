#!/bin/sh

dirname=`/usr/bin/dirname "$0"` || exit

sed=/usr/bin/sed

sys=`/usr/bin/uname -s` || sys=`/bin/uname -s` || exit
case "$sys" in
    SunOS)
	pspre='/bin/ps -o pid,ppid'
	pssuf='-o comm'
	user=-e	# the e option takes precedence over u
	with=1	# don't show sched, init, pageout, and fsflush
	;;
    Linux)	# a lot like Solaris
	pspre='/bin/ps -o pid,ppid'
	pssuf='-o comm'
	user=-e	# the e option takes precedence over u
	with=0
	sed=/bin/sed
	;;
    *)	# Darwin, FreeBSD, etc
	pspre='/bin/ps -o pid,ppid -ax'
	pssuf='-o command'
	user=''
	with=0
esac

case $# in
    0)	# no args, just run with the defaults
	;;
    *)
	case "$1" in
	    -*)	# an argument for ps, pass it on
		;;
	    [0123456789]*)
		# pid to include in the tree
		with="$1"
		shift
		;;
	    *)	# otherwise a username
		case "$sys" in
		    SunOS | Linux)
			uid=`/usr/bin/id | \
			    /usr/bin/awk -F= 'BEGIN { RS=" " }
				/^[Uu][Ii][Dd]=/ { print $2 }' | \
			    "$sed" 's/[^0-9].*//'`
			user="-u $uid"
			;;
		    *)	# Darwin, FreeBSD, etc
			user="-U $1"
			;;
		esac

		shift
		;;
	esac
	;;
esac


$pspre $user "$@" $pssuf | \
    "$dirname"/otree id="PID" pid="PPID" infon=3 with="$with" headn=1
