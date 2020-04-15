# `.zshenv' is sourced on all invocations of the shell, unless the -f option is set. It should contain commands to set the command search path, plus other important environment variables. `.zshenv' should not contain commands that produce output or assume the shell is attached to a tty.

#path=( ~ $lpath ~/bin /usr/local /usr/local/bin/X11 /usr/bin /bin
#        /usr/ucb /usr/etc /usr/bin/X11 /usr/lib /usr/lib/X11
#        /usr/local/bin /etc /usr/openwin/bin /usr/gnu/bin
#        /usr/sbin /opt/gnu/bin /opt/local/bin
#        /usr/X11/bin /opt/lotus/bin /opt/Acrobat3/bin ~/notesr4 . )

#manpath=(/usr/man /usr/local/man)
#export MANPATH

#export LD_LIBRARY_PATH=/usr/dt/lib:/usr/openwin/lib:/opt/lotus/notes/latest/sunspa
#export GR_HOME=/usr/local/bin

###################
#   OS Specific   #
###################
case $(uname) in
Darwin)  # Darwin Environment
echo ".darwin zshenv loaded"

path=( ~ ~/bin /Applications/Emacs.app/Contents/MacOS
  /Applications/Emacs.app/Contents/MacOS/bin
  /Applications/Xcode.app/Contents/Developer/Tools /usr/local/bin
  /usr/local/sbin /usr/local/opt/openssl/bin /usr/local/opt/ruby/bin
  /usr/bin /bin /usr/sbin /sbin /opt/X11/bin /usr/local/opt/qt/bin )

manpath=( /usr/local/share/man /usr/local/man /usr/share/man /usr/X11/man )

export TMPDIR=/tmp
;; # end Darwin

Linux)  # Based off of Ubuntu
echo ".linux zshenv loaded"

path=(~ ~/bin ~/scripts ~/.local/bin /usr/local/sbin /usr/local/bin
  /usr/sbin /usr/bin /sbin /bin /usr/games /opt/jdk /opt/jdk/bin
  /usr/java/bin /usr/local/java/bin )

manpath=( /usr/local/share/man /usr/share/man /usr/X11R6/man )

export TMPDIR=/tmp
;; # end Linux

*)
echo "profile uname not reporing Darwin or Linux.  Where are we?"
;;

esac  # End System Specific case statement
