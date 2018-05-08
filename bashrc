# -*- shell-script -*-
# ~/.bashrc - executed for interactive non-login shells
#
# ------------------------------------------------------------------------------

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

#####################
#  Common Settings  #
#####################
set bell-style visible
bind 'set bell-style visible'		# No beeping
#bind 'set horizontal-scroll-mode on'	# Don't wrap
bind 'set show-all-if-ambiguous on'	# Tab once for complete
bind 'set visible-stats on'		# Show file info in complete

# vagrant on Fedora 21 wants to use libvirt, force it to virtualbox
export VAGRANT_DEFAULT_PROVIDER=virtualbox

#NNTPSERVER=nntp.aioe.org
NNTPSERVER=news.eternal-september.org
MORE=p
LESS="-XgmR"

umask 022
test -t 0 && stty erase '^?'	# changed from ^h because of emacs help
stty -ixon                      # disable ^Q and ^S flow control
set -o emacs
GZIP="-9"


#############################
#  Configure shell history  #
#############################
#export PROMPT_COMMAND="history -a; history -n"  # Manually update .bash_history file
export HISTCONTROL=ignorespace
#export HISTIGNORE="&:ls:[bf]g:exit"
#export HISTTIMEFORMAT="%Y-%m-%d %T "
unset HISTFILESIZE
export HISTSIZE=10000


####################
#  Bash Variables  #
####################
set -o noclobber	# disable > >& <> from overwriting existing files
#set -o physical
shopt -s cdspell	# corrects for slop in directory spelling
shopt -s checkwinsize	# Keep COLUMNS and LINES updated
shopt -s extglob	# enable *(...), +(...), @(...), ?(...), !(...)
shopt -s dotglob	# include files beginning with a . in file expansion
shopt -s cmdhist	# save all lines of a multiple-line command
shopt -s histappend	# history list is appended to the history file
shopt -s cmdhist        # shell attempts to save each line of a multi-line command
shopt -s histreedit	# user can to re-edit a failed history substitution
shopt -s histverify	# history substitution loaded into readline buffer
shopt -s checkhash	# check hash table for command before executing it
shopt -s checkwinsize   # check term row/column size after each command before prompt


###############
#  Functions  #
###############
function my_ps { ps $@ -u $USER -o pid,%cpu,%mem,bsdtime,command; }
function psg { ps -aef | grep $* | grep -v grep; }  # sysv ps
function psb { ps aux | grep $* | grep -v grep; }  # bsd ps
function lhd { last $* | head; }
function calc { awk "BEGIN{ print $* }"; }
function rmd { pandoc $1 | lynx -stdin ; }

# get mac addr
function mac { ping -c 2 $1 > /dev/null 2>&1; arp $1 | awk '{print $3}' | tail -1; }

rot13 () {	# For some reason, rot13 pops up everywhere
    if [ $# -eq 0 ]; then
	tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    else
	echo $* | tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    fi
}

# Top 10 most used commands in history (TODO update for MacOS)
top10 () { history | awk '{print $2}' | awk 'BEGIN {FS="|"} {print $1}' | sort | uniq -c | sort -nr | head -10; }

# History unique grep search / to re-use a line found !123:p / !123
hugs () { history | grep -i -- "$1" | sort -k2 -u | grep -v 'hugs' | sort -n; }

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

# Repeat last command with sudo
yolo() {
    if [[ $# == 0 ]]; then
        sudo $(history -p '!!')
    else
        sudo "$@"
    fi
}

# ssh key functions
ssh-add-all() { LIST=$(ls $HOME/.ssh/id_* | grep -v '.pub'); ssh-add $LIST; } # add all ssh keys
ssh-del-all() { ssh-add -D; }                                       # delete all ssh keys
ssh-add-work () { ssh-add ${HOME}/.ssh/id_rsa_work; }               # add work key
ssh-del-work () { ssh-add -d ${HOME}/.ssh/id_rsa_work; }            # delete work key
ssh-add-github () { ssh-add ${HOME}/.ssh/github/id_rsa_github; }    # add github key
ssh-del-github () { ssh-add -d ${HOME}/.ssh/github/id_rsa_github; } # delete github key

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


#############
#  Aliases  #
#############
alias j='jobs'
alias h='history'
alias la='ls -a'
alias lf='ls -F'
alias ll='ls -l'
alias l.='ls -d .*'             # List only file beginning with "."
alias ll.='ls -ld .*'           # Long list only file beginning with "."
alias l='ls -laF'
alias lm='ls -laF | more'
alias lk='ls -lSr'              # sort by size in K, smallest to largest
alias lh='ls -lhSr'             # sort by size in human readable, smallest to largest
alias lr='ls -lR'               # recursice ls
alias lt='ls -ltr'              # sort by date
alias lla='ls -la'
alias lll='ls -actl | more'     # pipe through 'more'
alias ldir="ls -l | egrep '^d'" # show only directories
alias lfile="ls -l | egrep -v '^d'" # show files only"
alias digs="dig +short"
alias killmercer='sudo $(history -p !!)'
alias just='sudo'
alias gtfo='exit'
alias ssh-add-home="ssh-add ~/.ssh/home/id_rsa"
#
alias u='cd ..'
alias uu='cd ../..'
alias uuu='cd ../../..'
alias uuuu='cd ../../../..'
alias splitpath='echo -e ${PATH//:/\\n}'
alias histg='history | grep '
alias +='pushd .'
alias _='popd'
alias dirf='find . -type d | sed -e "s/[^-][^\/]*\//  |/g" -e "s/|\([^ ]\)/|-\1/'
# Deletes the last thing typed from history so it doesn't get written to .bash_history on exit
alias scratch='history -d $((HISTCMD-2)) && history -d $((HISTCMD-1))'
alias keepalive='while true; do date; sleep 120; done'
if [ `which watch` ]; then alias nap="watch -n 120 echo ${HOSTNAME}"; fi
# Re-attach to a screen session with updated ssh authentication
alias screenssh='ln -sf $SSH_AUTH_SOCK $HOME/.ssh-auth-sock; env SSH_AUTH_SOCK=$HOME/.ssh-auth-sock screen'
alias git_repo_name='git remote show -n origin | grep Fetch | cut -d: -f2-'
alias ping4='ping -c4'
alias weather='curl wttr.in/chicago'
alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test100.zip'
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"

# Proksel's aliases
alias aspen='tree -h -f -C'
alias atomize='open . -a Atom'
alias killmercer='sudo $(history -p !!)'
alias public_ip='curl ipecho.net/plain; echo'
alias terraform_graph='terraform graph | dot -Tpng > graph.png'

# git aliases
alias gg="git grep"
alias git-unfuck="git reset --hard HEAD"
alias gitGraph='git log --graph --oneline --all --decorate --color'


################
#  Set Prompt  #
################

# Name the colors
BLACK="\[\033[0;30m\]"   #Regular colors
RED="\[\033[0;31m\]"
GREEN="\[\033[0;32m\]"
YELLOW="\[\033[0;33m\]"
BLUE="\[\033[0;34m\]"
PURPLE="\[\033[0;35m\]"
CYAN="\[\033[0;36m\]"
WHITE="\[\033[0;37m\]"
#
BBLACK="\[\033[1;30m\]"   #Bold colors
BRED="\[\033[1;31m\]"
BGREEN="\[\033[1;32m\]"
BYELLOW="\[\033[1;33m\]"
BBLUE="\[\033[1;34m\]"
BPURPLE="\[\033[1;35m\]"
BCYAN="\[\033[1;36m\]"
BWHITE="\[\033[1;37m\]"
#
RESET="\[\033[0m\]" #Reset colors

# From Andrew Proksel
# get current branch in git repo
function parse_git_branch() {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]
    then
        STAT=`parse_git_dirty`
        echo "[${BRANCH}${STAT}]"
    else
        echo ""
    fi
}

# get current status of git repo
function parse_git_dirty {
    status=`git status 2>&1 | tee`
    dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
    untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
    ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
    newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
    renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
    deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
    bits=''
    if [ "${renamed}" == "0" ]; then
        bits=">${bits}"
    fi
    if [ "${ahead}" == "0" ]; then
        bits="*${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
        bits="+${bits}"
    fi
    if [ "${untracked}" == "0" ]; then
        bits="?${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
        bits="x${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
        bits="!${bits}"
    fi
    if [ ! "${bits}" == "" ]; then
        echo " ${bits}"
    else
        echo ""
    fi
}

# Shell Prompt
# if CHIC02RR812G8WP change to thorn
if [[ ${HOSTNAME} == "CHIC02RR812G8WP.grubhub.local" ]];then
  PS1="${YELLOW}\u${RESET}${WHITE}@thorn ${RESET}${CYAN}[\w]${RESET}${WHITE} \`parse_git_branch\`\n${WHITE}\$${RESET} "
else
  PS1="${YELLOW}\u${RESET}${WHITE}@\h ${RESET}${CYAN}[\w]${RESET}${WHITE} \`parse_git_branch\`\n${WHITE}\$${RESET} "
fi
export PS1


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
if [[ ! -z $PS1 ]]; then echo ".darwin bashrc loaded"; fi  # Interactive

if [[ $INSIDE_EMACS ]]; then
  echo "..Inside Emacs"
  TERM='vt100'
  #alias ls='ls --color=none'
  #alias grep='grep'
else
  TERM=xterm-256color
fi

export CLICOLOR=1

alias ldd="otool -L"
alias vmstat="vm_stat"
alias truss="dtruss"
alias flushdns="dscacheutil -flushcache"
alias restart-vpn="sudo /System/Library/StartupItems/CiscoVPN/CiscoVPN restart"
alias lsrebuild="/System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister -kill -r -domain local -domain system -domain user"  #rebuild launchservices database
alias mousebattery="ioreg -n "AppleBluetoothHIDMouse" | grep -i "batterypercent" | sed 's/[^[:digit:]]//g'"
alias spotlight-disable="sudo launchctl unload /System/Library/LaunchDaemons/com.apple.metadata.mds.plist"
alias spotlight-enable="sudo launchctl load /System/Library/LaunchDaemons/com.apple.metadata.mds.plist"
alias randompw="jot -r -c 160 . z | rs -g 0 10"
alias ddim="sudo pmset -a halfdim 0"	# turns off Mac display dimming
alias dsclean='find . -name .DS_Store -delete \{\} \;'
alias reindex="sudo mdutil -E /"  # erase spotlight indexes and rebuild
alias vlc='/Applications/VLC.app/Contents/MacOS/VLC'
alias ovftool='/Applications/VMware\ Fusion.app/Contents/Library/VMware\ OVF\ Tool/ovftool'
alias airportcycle='networksetup -setairportpower airport off; networksetup -setairportpower airport on'
alias airport='/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport'

###
# Configure Emacs and Emacsclient
# adapted from http://philipweaver.blogspot.com/2009/08/emacs-23.html
###
#alias emacs="Emacs"
EMACS_SOCKET="${HOME}/.emacs.d/tmp/server"
alias ecw="emacsclient -s $EMACS_SOCKET -n -c -a emacs" # start a windowed frame
alias ect="emacsclient -s $EMACS_SOCKET -t -a emacs -nw" # start a terminal frame
alias ec="emacsclient -s $EMACS_SOCKET -n -a emacs" # do not start a new frame

export EDITOR="${HOME}/bin/edit"
export ALTERNATE_EDITOR="zile"
export GROOVY_HOME=/usr/local/opt/groovy/libexec

function ediff {
    emacs --eval "(ediff \"$1\" \"$2\")"
}

function q { w3m -dump "http://google.com/search?q=$*" | more; }
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

# pyenv darwin
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi
;; # end Darwin

Linux)  # Based off of Ubuntu
if [[ ! -z $PS1 ]]; then echo ".linux bashrc loaded"; fi	# interactive

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

## pass options to free ##
alias meminfo='free -m -l -t'

## get top process eating memory
alias psmem='ps auxf | sort -nr -k 4'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'

## get top process eating cpu ##
alias pscpu='ps auxf | sort -nr -k 3'
alias pscpu10='ps auxf | sort -nr -k 3 | head -10'

## Get server cpu info ##
alias cpuinfo='lscpu'

# Status (from Rackspace)
function stats() { uptime; awk '/^MemTotal:/{total = $2/1024^2} /^(MemFree|Buffers|Cached):/{sum += $2} END {printf " Memory: %.2fG/%.2fG\n", sum/1024^2, total}' /proc/meminfo; ps -eo pcpu | awk '/[0-9]/ {sum += $1} END {printf " CPU: %s%%\n", sum}'; }

alias fullline='perl -p00e "s/\r?\n //gi"'
alias ls='ls --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias truss="strace"
alias about='cat /etc/redhat-release && cat /proc/version && uname -a'

# Debian
alias agc='df -h; apt-get autoclean ; apt-get clean ; apt-get autoremove ; df -h'
alias agi='apt-get install '
alias acs='apt-cache search '
alias agdu='apt-get update ; apt-get dist-upgrade'
lsmac() { /sbin/ifconfig -a | /bin/sed '/eth\|wl/!d;s/ Link.*HWaddr//' ; }

# Convert linux/redhat dmesg entry time stamps to human readable
dmesg_with_human_timestamps () {
    $(type -P dmesg) "$@" | perl -w -e 'use strict;
        my ($uptime) = do { local @ARGV="/proc/uptime";<>}; ($uptime) = ($uptime =~ /^(\d+)\./);
        foreach my $line (<>) {
            printf( ($line=~/^\[\s*(\d+)\.\d+\](.+)/) ? ( "[%s]%s\n", scalar localtime(time - $uptime + $1), $2 ) : $line )
        }'
}
alias dmesg=dmesg_with_human_timestamps

# OS X like pbcopy/pbpaste.
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

googlesay(){ curl -A RG translate\.google\.com/translate_tts -d "tl=en&q=$@" |mpg123 -; };

# pyenv linux
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
;; # end Linux

*)
echo "uname not reporing Darwin or Linux.  Where are we?"
;;

esac  # End System Specific case statement
