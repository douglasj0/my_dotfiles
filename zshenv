# -*- mode: sh -*-
# `.zshenv' is sourced on all invocations of the shell, unless the -f option is set. It should contain commands to set the command search path, plus other important environment variables. `.zshenv' should not contain commands that produce output or assume the shell is attached to a tty.

###################
#   OS Specific   #
###################
case $(uname) in
Darwin)  # Darwin Environment
[ ! -z "$PS1" ] && echo ".darwin zshenv loaded"

# disable reading of /etc/zprofile (global profiles) on MacOSX it changes path order
setopt no_global_rcs
# might need to add 'setopt global_rcs' to ~/.zprofile to re-enable for /etc/zshrc and /etc/zlogin

# Source .profile if readable, shared between shells
[[ -r ~/.profile ]] && source ~/.profile # shared PATH setup

#path=( ~ ~/bin
#  /Applications/Emacs.app/Contents/MacOS
#  /Applications/Emacs.app/Contents/MacOS/bin
#  /Applications/Xcode.app/Contents/Developer/Tools
#  /usr/local/lib/ruby/gems/3.0.0/bin /usr/local/opt/ruby/bin
#  /usr/local/bin /usr/local/sbin /usr/local/opt/openssl/bin
#  /usr/bin /bin /usr/sbin /sbin /opt/X11/bin /usr/local/opt/qt/bin
#  $path )
#
#manpath=( /usr/local/share/man /usr/local/man /usr/share/man /usr/X11/man )
#
## export to sub-processes (make it inherited by child processes)
#export PATH MANPATH TMPDIR=/tmp

;; # end Darwin

Linux)  # Based off of Ubuntu
[ ! -z "$PS1" ] && echo ".linux zshenv loaded"

[[ -r ~/.profile ]] && source ~/.profile # shared PATH setup

#path=( ~ ~/bin ~/scripts ~/.local/bin /usr/local/sbin /usr/local/bin
#  /usr/sbin /usr/bin /sbin /bin /usr/games /opt/jdk /opt/jdk/bin
#  /usr/java/bin /usr/local/java/bin )
#
#manpath=( /usr/local/share/man /usr/share/man /usr/X11R6/man )
#
#export TMPDIR=/tmp
;; # end Linux

*)
echo "profile uname not reporing Darwin or Linux.  Where are we?"
;;

esac  # End System Specific case statement
