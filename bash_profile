# -*- shell-script -*-
# .bash_profile, modified 4/22/2007 - dbj
#
# ~/.bash_profile - executed for login shells
# ~/.bashrc - executed for interactive non-login shells

###################
# System Specific #
###################
case $(uname) in
Darwin)  # Darwin Environment
echo ".Darwin bash_profile loaded"

PATH=$HOME/bin:${HOME}/Library/Python/2.7/bin/:/Applications/Emacs.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Xcode.app/Contents/Developer/Tools:/usr/local/bin:/usr/local/sbin:/usr/local/opt/openssl/bin:/usr/local/opt/python/libexec/bin/:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin:/usr/local/opt/qt/bin
MANPATH=/usr/local/share/man:/usr/local/man:/usr/share/man:/usr/X11/man
;; # end Darwin

Linux)  # Based off of Ubuntu
echo ".Linux bash_profile loaded"

PATH=${HOME}/bin:${HOME}/scripts:${HOME}/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/opt/jdk:/opt/jdk/bin:/usr/java/bin:/usr/local/java/bin
MANPATH=/usr/local/share/man:/usr/share/man:/usr/X11R6/man
;; # end Linux

*)
echo "bash_profile uname not reporing Darwin, SunOS, FreeBSD, or Linux.  Where are we?"
;;

esac  # End System Specific case statement

# Source .bashrc if it exists and is larger then zero
[[ -s ~/.bashrc ]] && source ~/.bashrc
