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

PATH=$HOME/bin:/Applications/Emacs.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Xcode.app/Contents/Developer/Tools:/usr/local/bin:/usr/local/sbin:/usr/local/opt/openssl/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/texbin:/usr/local/opt/qt/bin
MANPATH=/usr/local/share/man:/usr/local/man:/usr/share/man:/usr/X11/man
TMPDIR=/tmp
export PATH MANPATH TMPDIR

# pyenv darwin
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
;; # end Darwin

Linux)  # Based off of Ubuntu
echo ".linux profile loaded"

PATH=${HOME}/bin:${HOME}/scripts:${HOME}/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/opt/jdk:/opt/jdk/bin:/usr/java/bin:/usr/local/java/bin
MANPATH=/usr/local/share/man:/usr/share/man:/usr/X11R6/man
TMPDIR=/tmp
export PATH MANPATH TMPDIR

# pyenv linux
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
;; # end Linux

*)
echo "bash_profile uname not reporing Darwin or Linux.  Where are we?"
;;

esac  # End System Specific case statement