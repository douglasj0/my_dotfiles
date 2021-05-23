# -*- shell-script -*-
# ~/.profile - executed for graphical logins
#
# ------------------------------------------------------------------------------

###################
#   OS Specific   #
###################
case $(uname) in
Darwin)  # Darwin Environment
#echo ".darwin profile loaded"

#For compilers to find ruby you may need to set:
#  export LDFLAGS="-L/usr/local/opt/ruby/lib"
#  export CPPFLAGS="-I/usr/local/opt/ruby/include"

#For pkg-config to find ruby you may need to set:
#  export PKG_CONFIG_PATH="/usr/local/opt/ruby/lib/pkgconfig"

PATH=$HOME/bin:/Applications/Emacs.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin:/Applications/Xcode.app/Contents/Developer/Tools:/usr/local/lib/ruby/gems/3.0.0/bin:/usr/local/opt/ruby/bin:/usr/local/bin:/usr/local/sbin:/usr/local/opt/openssl/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin
MANPATH=/usr/local/share/man:/usr/local/man:/usr/share/man:/usr/X11/man
TMPDIR=/tmp
export PATH MANPATH TMPDIR

# pyenv path setup
if command -v ~/.pyenv/bin/pyenv 1>/dev/null 2>&1; then
  #echo "..pyenv path setup"
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
fi
;; # end Darwin

Linux)  # Based off of Ubuntu
echo ".linux profile loaded"

PATH=${HOME}/bin:${HOME}/scripts:${HOME}/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/opt/jdk:/opt/jdk/bin:/usr/java/bin:/usr/local/java/bin
MANPATH=/usr/local/share/man:/usr/share/man:/usr/X11R6/man
TMPDIR=/tmp

export PATH MANPATH TMPDIR

# pyenv path setup
if command -v ~/.pyenv/bin/pyenv 1>/dev/null 2>&1; then
  #echo "..pyenv path setup"
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init --path)"
fi
;; # end Linux

*)
echo "profile uname not reporing Darwin or Linux.  Where are we?"
;;

esac  # End System Specific case statement
