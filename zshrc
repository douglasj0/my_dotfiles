#
# my rc file for zsh 2.2
# all this runs in interactive shells only
#

# where to look for function definitions
# fpath=(~/func)

# use hard limits, except for a smaller stack and no core dumps
unlimit
limit stack 8192
limit core 0

umask 077

# setenv for csh junkies (including tset)
setenv() { export $1=$2 }

manpath=(/usr/man /usr/local/man)
export MANPATH

export LD_LIBRARY_PATH=/usr/dt/lib:/usr/openwin/lib:/opt/lotus/notes/latest/sunspa
export GR_HOME=/usr/local/bin

PROMPT='%T %m[%h]%# '

# functions to autoload
# autoload cx acx mere yu yp randline proto namedir ilogin
autoload -U compinit #autocompletion on hosts and usernames
compinit

MAILCHECK=30
HISTSIZE=600
DIRSTACKSIZE=50

setopt notify cdablevars autolist \
        sun_keyboard_hack auto_cd recexact long_list_jobs \
        hist_ignore_dups no_clobber \
        extended_glob rc_quotes nobeep
unsetopt bgnice

# Turn on auto completetion (ssh, ssh with user, etc)
autoload -U compinit
compinit

# some nice bindings
bindkey '^X^Z' universal-argument ' ' magic-space
bindkey '^X^A' vi-find-prev-char-skip
bindkey '^Z' accept-and-hold
bindkey -s '\M-/' \\\\
bindkey -s '\M-=' \|

# jsamuels stuff

# PROMPT="%m:%3c[$SHLVL]>"
# RPROMPT="%B%*%b"
HISTFILE=~/.zhistory
SAVEHIST='500'
HISTSIZE='500'
# DIRSTACKSIZE=50
# WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
# stty intr '^C'
# chpwd () { print -Pn '' }
# chpwd () { print -Pn '' }
# cd
psg () { ps -aef | grep $* | grep -v grep }
# stty erase ^H
stty -echoprt

# Functions
# repeat last command with sudo
function fucking {
     LAST_CMD=`fc -nl -1`
     echo sudo $LAST_CMD
     sudo zsh -c $LAST_CMD
}
