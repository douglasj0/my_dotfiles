# -*- shell-script -*-
# ~/.bash_profile - executed for login shells
#
# ------------------------------------------------------------------------------

###################
# Common Settings #
###################
# vagrant on Fedora 21 wants to use libvirt, force it to virtualbox
export VAGRANT_DEFAULT_PROVIDER=virtualbox

# Get results of uname for use later
UNAME=$(uname)
#NNTPSERVER=nntp.aioe.org
NNTPSERVER=news.eternal-september.org
[ -z $TERM ] && TERM=xterm-color
MORE=p
LESS="-XgmR"
[[ "x$EDITOR" == "x" ]] && export EDITOR="zile"  # set EDITOR if blank
umask 022
test -t 0 && stty erase '^?'	# changed from ^h because of emacs help
stty -ixon                      # disable ^Q and ^S flow control
set -o emacs
GZIP="-9"


###################
#   OS Specific   #
###################
case $(uname) in
Darwin)  # Darwin Environment
echo ".darwin bash_profile loaded"
;; # end Darwin

Linux)  # Based off of Ubuntu
echo ".linux bash_profile loaded"
;; # end Linux

*)
echo "bash_profile uname not reporing Darwin, SunOS, FreeBSD, or Linux.  Where are we?"
;;

esac  # End System Specific case statement

# Source .profile and .bashrc if they exist and are larger then zero
[[ -s ~/.profile ]] && source ~/.profile
[[ -s ~/.bashrc ]] && source ~/.bashrc
