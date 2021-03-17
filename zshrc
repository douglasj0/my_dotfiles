# -*- mode: sh -*-
#
# my rc file for zsh 2.2
# all this runs in interactive shells only
#

# `.zshrc' is sourced in interactive shells. It should contain commands to set up aliases, functions, options, key bindings, etc.

# where to look for function definitions
# fpath=(~/func)

# use hard limits, except for a smaller stack and no core dumps
unlimit
limit stack 8192
limit core 0

umask 077


# prompt colors
autoload -U colors && colors

# zsh add git branch if available
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '(%b)'

# Set prompt, was PROMPT='%T %m[%h]%# ', reset color %{$reset_color%}%
if [[ $(echo $HOST | grep "b.local") ]]; then MYHOST="b.local"; fi
if [[ ${MYHOST} == "b.local" ]]; then
PROMPT='%F{yellow}%T %n@thorn[%h]%f %F{cyan}[%~]%f %F{green}${vcs_info_msg_0_}%f
%F{white}%# %f'
elif [[ ${HOST} == "flowers" ]]; then
PROMPT='%F{green}%T %n@flowers[%h]%f %F{cyan}[%~]%f %F{yellow}${vcs_info_msg_0_}%f
%F{white}%# %f'
elif [[ ${HOST} == "lothlorien.local" ]]; then
PROMPT='%F{yellow}%T %n@lothlorien[%h]%f %F{cyan}[%~]%f %F{greeb}${vcs_info_msg_0_}%f
%F{white}%# %f'
else
PROMPT='%T %m[%h] [%~] ${vcs_info_msg_0_}
%# '
fi

# Prevent text pasted into the terminal from being highlighted
# Introduced in zsh 5.1
zle_highlight+=(paste:none)

# functions to autoload
# autoload cx acx mere yu yp randline proto namedir ilogin
autoload -Uz compinit && compinit #autocompletion on hosts and usernames
# small letters will match small and capital letters. (i.e. capital letters match only capital letters.)
#zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
# capital letters also match small letters use instead:
#zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# If you want case-insensitive matching only if there are no case-sensitive matches add '', e.g.
#zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'


# https://www.viget.com/articles/zsh-config-productivity-plugins-for-mac-oss-default-shell/
# History
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
HISTSIZE=50000
SAVEHIST=10000
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt inc_append_history
#setopt share_history
setopt no_share_history
# Changing directories
setopt auto_cd
setopt auto_pushd
unsetopt pushd_ignore_dups
setopt pushdminus
# Completion
setopt auto_menu
setopt always_to_end
setopt complete_in_word
unsetopt flow_control
unsetopt menu_complete
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH_CACHE_DIR
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
# Other
setopt prompt_subst


#MAILCHECK=30
#HISTSIZE=600
#DIRSTACKSIZE=50

setopt notify cdablevars autolist \
        sun_keyboard_hack auto_cd recexact long_list_jobs \
        hist_ignore_dups no_clobber \
        extended_glob rc_quotes nobeep
unsetopt bgnice

# Turn on auto completetion (ssh, ssh with user, etc)
autoload -U compinit
compinit

# some nice bindings
#bindkey '^X^Z' universal-argument ' ' magic-space
#bindkey '^X^A' vi-find-prev-char-skip
#bindkey '^Z' accept-and-hold
#bindkey -s '\M-/' \\\\
#bindkey -s '\M-=' \|

# jsamuels stuff

# PROMPT="%m:%3c[$SHLVL]>"
# RPROMPT="%B%*%b"
#HISTFILE=~/.zhistory
#SAVEHIST='1000'
#HISTSIZE='1000'
# DIRSTACKSIZE=50
# WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
# stty intr '^C'
# chpwd () { print -Pn '' }

# cd

# stty erase ^H
stty -echoprt

NNTPSERVER=news.eternal-september.org
MORE=p
LESS="-XgmR"

# Load general aliases
source $HOME/.dotfiles/shell_aliases

# Functions
# setenv for csh junkies (including tset)
seten() { export $1=$2 }

# decode base64 from stdin
decode64 () {
  echo "$1" | base64 -d | pbcopy
}
dbldecode64 () {
  echo "$1" | base64 -d | base64 -d | pbcopy
}
encode64 () {
  echo "$1" | base64 ; echo
}

# repeat last command with sudo
fucking() {
     LAST_CMD=`fc -nl -1`
     echo sudo $LAST_CMD
     sudo zsh -c $LAST_CMD
}

psg () { ps -aef | grep $* | grep -v grep }

flip() {
  echo;
  echo -en "( º_º）  ┬─┬   \r"; sleep .5;
  echo -en " ( º_º） ┬─┬   \r"; sleep .5;
  echo -en "  ( ºДº）┬─┬   \r"; sleep .5;
  echo -en "  (╯'Д'）╯︵⊏   \r"; sleep .5;
  echo -en "  (╯'□'）╯︵ ⊏  \r"; sleep .5;
  echo     "  (╯°□°）╯︵ ┻━┻"; sleep .5;
}

calc() { awk "BEGIN{ print $* }"; }
delhost() { ssh-keygen -R $@; }  # remove entry from ~/.ssh/known_hosts

rot13() {  # For some reason, rot13 pops up everywhere
    if [ $# -eq 0 ]; then
	tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    else
	echo $* | tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    fi
}

# Top 10 most used commands in history (TODO update for MacOS)
top10 () { history | awk '{print $2}' | awk 'BEGIN {FS="|"} {print $1}' | sort | uniq -c | sort -nr | head -10; }

## Extract common archive formats
extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1    ;;
            *.tar.gz)    tar xvzf $1    ;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1      ;;
            *.tar)       tar xvf $1     ;;
            *.tbz2)      tar xvjf $1    ;;
            *.tgz)       tar xvzf $1    ;;
            *.zip)       unzip $1       ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1        ;;
            *)           echo "don't know how to extract '$1'..." ;;
        esac
    else
        echo "'$1' is not a valid file!"
    fi
}

# ssh key functions
ssh-add-all() { LIST=$(ls $HOME/.ssh/id_* | grep -v '.pub'); ssh-add $LIST; } # add all ssh keys
ssh-del-all() { ssh-add -D; }                              # delete all ssh keys
ssh-add-work () { ssh-add ${HOME}/.ssh/id_rsa_work; }      # add work key
ssh-del-work () { ssh-add -d ${HOME}/.ssh/id_rsa_work; }   # delete work key
ssh-add-home () { ssh-add ${HOME}/.ssh/id_rsa_home; }      # add home github key
ssh-del-home () { ssh-add -d ${HOME}/.ssh/id_rsa_home; }   # delete home github key

#grepp: grep by paragraph, http://www.commandlinefu.com/commands/view/4547/
grepp() {
    [ $# -eq 1 ] && perl -00ne "print if /$1/i" || perl -00ne "print if /$1/i" < "$2"
}

# Push ssh authorized_keys to remote host
pushkey() {
  if [ -z $1 ]; then
     echo "no host specified"
  else
    KEYCODE=`cat $HOME/.ssh/authorized_keys`
    ssh -q $1 "mkdir -p ~/.ssh && chmod 0700 ~/.ssh && touch ~/.ssh/authorized_keys && echo "$KEYCODE" >> ~/.ssh/authorized_keys && chmod 644 ~/.ssh/authorized_keys"
  fi
}

# magit, function to open magit buffer from current git repo
magit() {
  if git status > /dev/null 2>&1; then
      #emacsclient -nw --eval "(call-interactively #'magit-status)"
      emacsclient -s ${HOME}/.emacs.d/tmp/server -n -a emacs --eval "(call-interactively #'magit-status)"
  else
      echo "Not in a git repo"
      return 1
  fi
}


###################
#  Source workrc  #
###################
if [ -e ${HOME}/.workrc ]; then
  source ~/.workrc
fi


###
# pyenv functions
###
rt-activate() {
  pyenv activate research-tools
  cd ~/projects/CBT/research_tools
}


###################
#   OS Specific   #
###################
case "$(uname)" in
Darwin)  # Darwin Environment
if [[ ! -z $PS1 ]]; then echo ".darwin zshrc loaded"; fi  # Interactive

# Load Darwin aliases
source $HOME/.dotfiles/darwin_shell_aliases

if [[ $INSIDE_EMACS ]]; then
  echo "..Inside Emacs"
  TERM='vt100'
  #alias ls='ls --color=none'
  #alias grep='grep'
else
  TERM=xterm-256color
fi

###
# Configure Emacs and Emacsclient
# adapted from http://philipweaver.blogspot.com/2009/08/emacs-23.html
###
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"  # lowercase bin/emacs is broken
EMACS_SOCKET="${HOME}/.emacs.d/tmp/server"
alias ecw="emacsEclient -s $EMACS_SOCKET -n -c -a emacs" # start a windowed frame
alias ect="emacsclient -s $EMACS_SOCKET -t -a emacs -nw" # start a terminal frame
alias ec="emacsclient -s $EMACS_SOCKET -n -a emacs" # do not start a new frame

export EDITOR="${HOME}/bin/edit"
export ALTERNATE_EDITOR="zile"
export GROOVY_HOME=/usr/local/opt/groovy/libexec

function ediff {
    emacs --eval "(ediff \"$1\" \"$2\")"
}

#function q { w3m -dump "http://google.com/search?q=$*" | more; }
function traffic { netstat -w1 -I"$@"; }
function qlook { qlmanage -p "$@" >& /dev/null & }

if [[ -f ~/Library/LaunchAgents/gnu.emacs.daemon.plist ]]; then
    alias emacs_load="launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
    alias emacs_unload="launchctl unload -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
    alias emacs_status="launchctl list | grep emacs"
fi

if [[ -f ~/Library/mysql/com.mysql.mysqld.plist ]]; then
    alias start_mysql="sudo launchctl load ~/Library/mysql/com.mysql.mysqld.plist"
    alias stop_mysql="sudo launchctl unload ~/Library/mysql/com.mysql.mysqld.plist"
fi

# pyenv local git install
if file ~/.pyenv/bin/pyenv > /dev/null; then PYENV_ROOT="$HOME/.pyenv"; PATH="$PYENV_ROOT/bin:$PATH"; eval "$(pyenv init -)"; fi
if file ~/.pyenv/plugins/pyenv-virtualenv/bin/pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# jenv darwin
if which jenv > /dev/null; then export PATH="$HOME/.jenv/bin:$PATH"; eval "$(jenv init -)"; fi
;; # end Darwin

Linux)  # Based off of Ubuntu
if [[ ! -z $PS1 ]]; then echo ".linux zshrc loaded"; fi # interactive

# Load Linux aliases
source $HOME/.dotfiles/linux_shell_aliases

## Open like command for Linux:  xdg-open or see
function open { xdg-open "$1" &> /dev/null & }

TERM=xterm-color

###
# Configure Emacs and Emacsclient
# adapted from http://philipweaver.blogspot.com/2009/08/emacs-23.html
###
alias ecw="emacsclient -n -c -a emacs" # start a windowed frame
alias ect="emacsclient -t -a emacs -nw" # start a terminal frame
alias ec="emacsclient -n -a emacs" # do not start a new frame
# export EDITOR="emacsclient -t"
[[ "x$EDITOR" == "x" ]] && export EDITOR="zile"  # set EDITOR if blank
export ALTERNATE_EDITOR="zile"

# Status (from Rackspace)
function stats() { uptime; awk '/^MemTotal:/{total = $2/1024^2} /^(MemFree|Buffers|Cached):/{sum += $2} END {printf " Memory: %.2fG/%.2fG\n", sum/1024^2, total}' /proc/meminfo; ps -eo pcpu | awk '/[0-9]/ {sum += $1} END {printf " CPU: %s%%\n", sum}'; }

lsmac() { /sbin/ifconfig -a | /bin/sed '/eth\|wl/!d;s/ Link.*HWaddr//' ; }

# Convert linux/redhat dmesg entry time stamps to human readable
dmesg_with_human_timestamps () {
    $(type -P dmesg) "$@" | perl -w -e 'use strict;
        my ($uptime) = do { local @ARGV="/proc/uptime";<>}; ($uptime) = ($uptime =~ /^(\d+)\./);
        foreach my $line (<>) {
            printf( ($line=~/^\[\s*(\d+)\.\d+\](.+)/) ? ( "[%s]%s\n", scalar localtime(time - $uptime + $1), $2 ) : $line )
        }'
}
alias dmesght=dmesg_with_human_timestamps

googlesay(){ curl -A RG translate\.google\.com/translate_tts -d "tl=en&q=$@" |mpg123 -; };

# pyenv linux
#export PYENV_ROOT="$HOME/.pyenv"
#export PATH="$PYENV_ROOT/bin:$PATH"
#eval "$(pyenv init -)"
#eval "$(pyenv virtualenv-init -)"
if file ~/.pyenv/bin/pyenv > /dev/null; then PYENV_ROOT="$HOME/.pyenv"; PATH="$PYENV_ROOT/bin:$PATH"; eval "$(pyenv init -)"; fi
if file ~/.pyenv/plugins/pyenv-virtualenv/bin/pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
;; # end Linux

*)
echo "uname not reporing Darwin or Linux.  Where are we?"
;;

esac  # End System Specific case statement
