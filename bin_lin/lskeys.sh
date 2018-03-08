#!/bin/bash
#: Title	: lskeys
#: Date Created : Thu Oct 27 22:44:20 PDT 2011
#: Last Edit	: Fri Nov 18 23:17:02 PST 2011
#: Author	: Agnelo de la Crotche (please_try_again)
#: Version	: 2.0
#: Description	: lists, exports, deletes RPM GPG keys
#: Syntax	: lskeys [option]
#: Options      : -e --export : exports selected key
#:              : -d --delete : ereases selected key 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

declare -l linux dist
declare -a K E N T S

if ( which lsb_release &>/dev/null ) ; then
	linux=$(lsb_release -si | tr -d " ")
else
	linux=$(sudo find /etc ! -name "meta*" ! -name "lsb*" ! -name "system*" ! -name "jpackage*" -name "*-release" -exec basename "{}" -release ";" 2>/dev/null | sort | head -1)
fi

case $linux in
fedora|suse|suselinux|mandrivalinux) dist=rpm ;;
ubuntu|linuxmint|debian) dist=apt ;;
*) exec echo "unsupported distro" ;;
esac

[ "x$1" == "x-e" -o "x$1" == "x--export" ] && EXPORT=1
[ "x$1" == "x-d" -o "x$1" == "x--delete" ] && DELETE=1

i=0

function listkeys {
	case $dist in
	rpm)
		for k in $(rpm -qa gpg-pubkey*) ; do
			let i++	; K[$i]=$k ; N[$i]=${k/gpg-pubkey-/}
			eval $(LC_ALL=C rpm -qi $k | gpg --with-key-data 2>/dev/null | awk -F ":" '/^pub/ { gsub(/\\x3/,":", $10) ; if ($6 ~ /-/ ) printf "%s;%s;", $6, $10 ; else printf "%s;%s;", strftime("%Y-%m-%d", $6), $10 ; if ( $7 ~ /-/ ) printf "%s" , $7 ; else if ( $7 ) printf "%s", strftime("%Y-%m-%d", $7) ; printf "\n" }' | sed "s|\(.*\);\(.*\);\(.*\)|T[$i]=\"\1\";S[$i]=\"\2\";E[$i]=\"\3\";|")
		done
	;;
	apt)
		eval $(LC_ALL=C apt-key list | sed -e '/^sub/d' | sed -e :a -e '$!N;s/\nuid */; /;ta' -e 'P;D' | sed -n 's/pub *//p' | sed 's/>;.*//;s| |;|;s|\(.*\) \[expires: \([0-9-]*\)\]; *\(.*\)|\1;\3;\2|;s|; *|;|g' | awk -F ";" '{ K=$1 ; sub(/.*\//, "", K) ; N=$1 ; sub(/\//, "-", N) ; printf "K[%i]=\"%s\";N[%i]=\"%s\";T[%i]=\"%s\";S[%i]=\"%s\";\n", NR, K, NR, N, NR, $2, NR, $3 ; if ($4) printf "E[%i]=\"\%s\";", NR, $4 }'  2>/dev/null)
	;;
	*) return 1 ;;
	esac
}

function viewkey {
	case $dist in
		rpm) rpm -qi $1 ;;
		apt) apt-key export $1 ;;
		*) return 1 ;;
	esac
}

function deletekey {
	case $dist in
		rpm) rpm -e $1 ;;
		apt) apt-key del $1 ;;
		*) return 1 ;;
	esac
}

function isExpired {
	keyTime=$(echo "${@//-/ } 00 00 00" | awk ' { print mktime($0) }')
	sysTime=$(awk 'BEGIN { print systime()}')
	[ $keyTime -lt $sysTime ] && return 0 || return 1
}

listkeys

i=1
while [ $i -le ${#N[@]} ] ; do
	KCL=$(tput setaf 7) ; TCL=$(tput sgr0) ; SCL=$(tput setaf 2)
	[ "${E[$i]}" ] && isExpired ${E[$i]} && { KCL=$(tput setaf 1); TCL=$(tput setaf 1) ; SCL=$(tput setaf 1); }
	printf "[%2s] %s%s%-20s%s%-13s%s%-13s%s%s%s\n" $i $(tput bold) $KCL ${N[$i]} $TCL "${T[$i]}" $(tput setaf 1) "${E[$i]}" $SCL "${S[$i]}" $(tput sgr0)
	let i++
done
let i--

declare -u j=0

while [ $j -lt 1 -o $j -gt $i ] ; do
	read -p "view key [1-$i or Q to quit]: " j
	[ "$j" == "Q" ] && break
	j=$(($j*1))
	echo
	[ "${K[$j]}" ] && viewkey ${K[$j]}
	if [ "$EXPORT" ] ; then
		GPG="$(echo $dist | tr "[:lower:]" "[:upper:]")GPG-${N[$j]}.key"
		echo $GPG		
		viewkey ${K[$j]} | sed -n '/BEGIN/,/END/p' > $GPG && printf "\nKey %s%s%s%s successfully written in %s%s/%s%s\n" $(tput bold) $(tput setaf 3) ${N[$j]} $(tput sgr0) $(tput bold) $(pwd) $GPG $(tput sgr0)
	elif [ "$DELETE" ] ; then
		declare -u YN
		YN=""
		while [ "x$YN" != "xY" -a "x$YN" != "xN" ] ; do
			read -p "Delete key ${K[$j]}? [y/n] " YN
			[ "$YN" == "Y" ] && deletekey ${K[$j]} && printf "\nKey %s%s%s%s successfully deleted.\n" $(tput bold) $(tput setaf 3) ${K[$j]} $(tput sgr0)
		done
	fi
done
