# -*- shell-script -*-
# ~/.profile - executed for graphical logins
#
# ------------------------------------------------------------------------------

###################
#   OS Specific   #
###################
case $(uname) in
Darwin)  # Darwin Environment
echo ".darwin profile loaded"

#For compilers to find ruby you may need to set:
#  export LDFLAGS="-L/usr/local/opt/ruby/lib"
#  export CPPFLAGS="-I/usr/local/opt/ruby/include"

#For pkg-config to find ruby you may need to set:
#  export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"

PATH=$HOME/bin:/Applications/Emacs.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Xcode.app/Contents/Developer/Tools:/usr/local/bin:/usr/local/sbin:/usr/local/opt/openssl/bin:/usr/local/opt/ruby/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin:/usr/local/opt/qt/bin
MANPATH=/usr/local/share/man:/usr/local/man:/usr/share/man:/usr/X11/man
TMPDIR=/tmp
export PATH MANPATH TMPDIR
;; # end Darwin

Linux)  # Based off of Ubuntu
echo ".linux profile loaded"

PATH=${HOME}/bin:${HOME}/scripts:${HOME}/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/opt/jdk:/opt/jdk/bin:/usr/java/bin:/usr/local/java/bin
MANPATH=/usr/local/share/man:/usr/share/man:/usr/X11R6/man
TMPDIR=/tmp
export PATH MANPATH TMPDIR
;; # end Linux

*)
echo "bash_profile uname not reporing Darwin or Linux.  Where are we?"
;;

esac  # End System Specific case statement
