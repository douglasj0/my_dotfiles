# -*- shell-script -*-
# .bashrc
#
# ~/.bash_profile - executed for login shells
# ~/.bashrc - executed for interactive non-login shells
#
# Modified:
# 20100512 - Merged built up changes from Mac and Solaris
# 20111007 - Changed prompts to single line, cleaned out deadweight
# 20120714 - Re-arranged, moved common settings to the top
# 20120803 - Removed ShopperTrak entries
# 20170104 - Removed ALL specific work related entries
# ------------------------------------------------------------------------------

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

###################
# Common Settings #
###################

# Get results of uname for use later
UNAME=$(uname)

# vagrant on Fedora 21 wants to use libvirt, force it to virtualbox
export VAGRANT_DEFAULT_PROVIDER=virtualbox

#NNTPSERVER=nntp.aioe.org
NNTPSERVER=news.eternal-september.org
umask 022
#stty sane
test -t 0 && stty erase '^?'	# changed from ^h because of emacs help
stty -ixon                      # disable ^Q and ^S flow control
#TERM=vt100
#[ -z $TERM ] && TERM=vt100	# if TERM isn't set make it vt100
#TERM=xterm
[ -z $TERM ] && TERM=xterm-color
MORE=p
#LESS="aCegj20mqX"
LESS="-XgmR"
[[ "x$EDITOR" == "x" ]] && export EDITOR="zile"  # set EDITOR if blank
#export ALTERNATE_EDITOR=emacs
#export VISUAL=emacsclient
#export SVN_EDITOR='"vi"'
set -o emacs
set bell-style visible
bind 'set bell-style visible'		# No beeping
#bind 'set horizontal-scroll-mode on'	# Don't wrap
bind 'set show-all-if-ambiguous on'	# Tab once for complete
bind 'set visible-stats on'		# Show file info in complete


###########################
# Configure shell history #
###########################
#export PROMPT_COMMAND="history -a; history -n"  # Manually update .bash_history file
export HISTCONTROL=ignorespace
#export HISTIGNORE="&:ls:[bf]g:exit"
#export HISTTIMEFORMAT="%Y-%m-%d %T "
unset HISTFILESIZE
export HISTSIZE=10000


##################
# Bash Variables #
##################
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
# Completions #
###############
# tab complete any hostname you've previously ssh'd to
# http://www.macosxhints.com/article.php?story=20080317085050719
#complete -W "$(sed -e 's/^  *//' -e '/^#/d' -e 's/[, ].*//' -e '/\[/d' ~/.ssh/known_hosts | sort -u)" ssh
_complete_ssh_hosts () {
        COMPREPLY=()
        cur="${COMP_WORDS[COMP_CWORD]}"
        comp_ssh_hosts=`cat ~/.ssh/known_hosts | \
                        cut -f 1 -d ' ' | \
                        sed -e s/,.*//g | \
                        grep -v ^# | \
                        uniq | \
                        grep -v "\[" ;
                if [ -f ~/.ssh/config ]; then
                        cat ~/.ssh/config | \
                                grep -i "^Host " | \
                                awk '{print $2}'
                fi`
        COMPREPLY=( $(compgen -W "${comp_ssh_hosts}" -- $cur))
        return 0
}
complete -F _complete_ssh_hosts ssh


#############
# Functions #
#############
function my_ps { ps $@ -u $USER -o pid,%cpu,%mem,bsdtime,command ; }
function psg { ps -aef | grep $* | grep -v grep ; }  # sysv ps
function psb { ps -aux | grep $* | grep -v grep ; }  # bsd ps
function du1 { du -s *(/) | sort -n ; }
function lhd { last $* | head ; }
function ? { echo "$*" | bc -l; }
function calc { awk "BEGIN{ print $* }" ;}
function rmd { pandoc $1 | lynx -stdin ;}

# Status (from Rackspace?)
function stats() { uptime; awk '/^MemTotal:/{total = $2/1024^2} /^(MemFree|Buffers|Cached):/{sum += $2} END {printf " Memory: %.2fG/%.2fG\n", sum/1024^2, total}' /proc/meminfo; ps -eo pcpu | awk '/[0-9]/ {sum += $1} END {printf " CPU: %s%%\n", sum}'; }

# get mac addr
function mac { ping -c 2 $1 > /dev/null 2>&1; arp $1 | awk '{print $3}' | tail -1; }

# Do quick arithmetic from the command line. Use "x" for multiplication
function math { echo "scale=2 ; $*" | sed -e "s:x:*:g" | sed -e "s:,::g" | bc; }

# GNU Screen, opens SSH on a new screen window with the appropriate name.
function screen_ssh {
    numargs=$#
    screen -t ${!numargs} ssh $@
#    screen -t "$@" ssh "$@"
}
#if [ $TERM == "screen" -o $TERM == "vt102" ]; then
[ -n "$STY" -o -n "$WINDOW" ] && alias sshs=screen_ssh

function tabname {  # Set the tab name in OSX Terminal
     printf "\e]1;$1\a"
}

findgrep () {	# find | grep
    if [ $# -eq 0 ]; then
        echo "findgrep: No arguments entered."; return 1
    else
        # "{.[a-zA-Z],}*" instead of "." makes the output cleaner
        find {.[a-zA-Z],}* -type f | xargs grep -n $* /dev/null \
        2> /dev/null
    fi
}

rot13 () {	# For some reason, rot13 pops up everywhere
    if [ $# -eq 0 ]; then
        tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    else
        echo $* | tr '[a-m][n-z][A-M][N-Z]' '[n-z][a-m][N-Z][A-M]'
    fi
}

dls () {	# print directories then files
    /bin/ls -lF "$@" | egrep '^d|total';
    #/bin/ls -lFXB "$@" | egrep -v '^d|total';
    /bin/ls -lFB "$@" | egrep -v '^d|total';
}

# prints a box around text
box () { t="$1xxxx";c=${2:-=}; echo ${t//?/$c}; echo "$c $1 $c"; echo ${t//?/$c}; }

# mkdir and enter it immediately thereafter
mcd () { mkdir -p $1 && cd $1; }

# cd and run ls
cdl () { builtin cd "$@" && ls; }

# Top 10 most used commands in history (TODO update for MacOS)
top10 () { history | awk '{print $2}' | awk 'BEGIN {FS="|"} {print $1}' | sort | uniq -c | sort -nr | head -10; }
top10a () { history | cut -f 5 -d' ' | sort | uniq -c | sort -n | tail; }

# History unique grep search / to re-use a line found !123:p / !123
hugs () { history | grep -i -- "$1" | sort -k2 -u | grep -v 'hugs' | sort -n ; }

# Add a .bak extension to a file
bak () {
    mv $1 $1.bak
}

# Remove a .back extension from a file
unbak () {
    length=$((${#1} - 4))
    mv $1 ${1:0:$length}
}

# Remove a host from the known_hosts file (Might be a better way...)
remove-known-host () {
    local HOST=$1
    grep -v $HOST ~/.ssh/known_hosts > ~/.ssh/known_hosts.tmp
    mv ~/.ssh/known_hosts.tmp ~/.ssh/known_hosts
}

# Extract common archive formats
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
#fuck() {
#    if [[ $# == 0 ]]; then
#        sudo $(history -p '!!')
#    else
#        sudo "$@"
#    fi
#}

# Tableflip animation
flip() {
  echo;
  echo -en "( º_º）  ┬─┬   \r"; sleep .5;
  echo -en " ( º_º） ┬─┬   \r"; sleep .5;
  echo -en "  ( ºДº）┬─┬   \r"; sleep .5;
  echo -en "  (╯'Д'）╯︵⊏   \r"; sleep .5;
  echo -en "  (╯'□'）╯︵ ⊏  \r"; sleep .5;
  echo     "  (╯°□°）╯︵ ┻━┻"; sleep .5;
}

# Bounce animation
bounce() {
  echo;
  echo -en '°'"\r"; sleep .1;
  echo -en '°º'"\r"; sleep .1;
  echo -en '°º¤'"\r"; sleep .1;
  echo -en '°º¤ø'"\r"; sleep .1;
  echo -en '°º¤ø,'"\r"; sleep .1;
  echo -en '°º¤ø,¸'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,ø'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,ø,'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,ø,¸'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,ø,¸,'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,ø,¸,¸'"\r"; sleep .1;
  echo -en '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,ø,¸,¸¸'"\r"; sleep .1;
  echo     '°º¤ø,¸¸,ø¤º°º¤ø,¸,ø¤º¤ø,¸,ø,¸,¸¸¸¸'; sleep .1;
}

# Make and cd into a directory
mkcd() {
    mkdir "$@" || return "$?"
    shift $(( $# - 1 ))
    cd "$1"
}

# Add all ssh keys in ~/.ssh
ssh-add-all() {
  LIST=$(ls $HOME/.ssh/id_* | grep -v '.pub')
  ssh-add $LIST
}

# Delete all ssh keys in keyring
ssh-del-all() {
  ssh-add -D
}

# Only add work key
ssh-add-work () {
  ssh-add ${HOME}/.ssh/id_rsa_work
}

# Only delete work key
ssh-del-work () {
  ssh-add -d ${HOME}/.ssh/id_rsa_work
}

# Only add github key
ssh-add-github () {
  ssh-add ${HOME}/.ssh/github/id_rsa_github
}

# Only delete github key
ssh-del-github () {
  ssh-add -d ${HOME}/.ssh/github/id_rsa_github
}

#grepp: grep by paragraph
#thx to http://www.commandlinefu.com/commands/view/4547/
grepp() {
    [ $# -eq 1 ] && perl -00ne "print if /$1/i" || perl -00ne "print if /$1/i" < "$2"
}

# Emacs Remote Directory Tracking (ansi-term)
function set-eterm-dir {
    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
    if [ $UNAME = "SunOS" ]; then
        if [[ $(grep savvis-chi3-il /etc/hosts) ]]; then
            domain="savvis-chi3-il.rctanalytics.com";
        else
            domain="rctanalytics.com";
        fi
        # The -f option does something else on SunOS and is not needed anyway.
        hostname_options="";
    else
        domain-"";
        hostname_options="-f";
    fi
    echo -e "\033AnSiTh" "$(hostname $hostname_options)" # Using the -f option can cause problems on some OSes.
    history -a # Write history to disk.
}

# Push ssh authorized_keys to remote host
pushkey() {
  #cat ~/.ssh/authorized_keys | \
  #ssh $1 "cat >> authorized_keys && mkdir -p ~/.ssh && chmod 0700 ~/.ssh && mv ~/authorized_keys ~/.ssh"
  if [ -z $1 ]; then
     echo "no host specified"
  else
    KEYCODE=`cat $HOME/.ssh/id_rsa_work.pub`
    ssh -q $1 "mkdir -p ~/.ssh && chmod 0700 ~/.ssh && touch ~/.ssh/authorized_keys && echo "$KEYCODE" >> ~/.ssh/authorized_keys && chmod 644 ~/.ssh/authorized_keys"
  fi
}


# Track directory, username, and cwd for remote logons.
#if [ "$TERM" = "eterm-color" ]; then
#if [ "x${INSIDE_EMACS}" != "x" ]; then
#if [ "${SSH_CLIENT%% *}" = "10.0.3.31" -o "${SSH_CLIENT%% *}" = "10.3.14.49" ]; then
#   PROMPT_COMMAND=set-eterm-dir
#   echo "We're inside Emacs"
#    echo "Run 'set-eterm-dir' to enable Emacs ansi-term directory tracking"
#fi

# Check if we're running inside of Emacs, change TERM to accomodate
#if [ -n "$INSIDE_EMACS" ]; then export TERM=vt100; fi
#[ "x$INSIDE_EMACS" != "x" ]] && export TERM=vt100


###########
# Aliases #
###########
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
alias gitGraph="git log --graph --oneline --all --decorate --color"
alias aspen="tree -h -f -C"
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

# about-alias 'the silver searcher (ag) aliases'
## Summary for args to less:
# less(1)
#   -M (-M or --LONG-PROMPT) Prompt very verbosely
#   -I (-I or --IGNORE-CASE) Searches with '/' ignore case
#   -R (-R or --RAW-CONTROL-CHARS) For handling ANSI colors
#   -F (-F or --quit-if-one-screen) Auto exit if <1 screen
#   -X (-X or --no-init) Disable termcap init & deinit
alias ag='ag --smart-case --pager="less -MIRFX"'

cdls() {
  builtin cd "$*" && ls
}
alias cdl="cdls"

# Proksel's aliases
alias aspen='tree -h -f -C'
alias atomize='open . -a Atom'
alias gitGraph='git log --graph --oneline --all --decorate --color'
alias killmercer='sudo $(history -p !!)'
alias public_ip='curl ipecho.net/plain; echo'
alias terraform_graph='terraform graph | dot -Tpng > graph.png'


###
# Load git-completion if git is installed
###
# Add the following lines to ~/.bashrc
# if [ -x `which git` ]; then
#     if [ -e ~/.git-completion.bash ]; then
# 	echo "Sourcing .git-completion.bash"
# 	source ~/.git-completion.bash
#     fi
# fi

##############
# Set Prompt #
##############

# http://brettterpstra.com/my-new-favorite-bash-prompt
# NOTE: double quotes to enable $color variable expansion and \[ \] escapes
#   around them so they are not counted as character positions and the cursor
#   position is not wrong)

# example:  08:53 djackson@orlok[5740/0]:~/Documents

#function set_prompt {
#    local BLACK="\[\033[0;30m\]"   #Regular colors
#    local RED="\[\033[0;31m\]"
#    local GREEN="\[\033[0;32m\]"
#    local YELLOW="\[\033[0;33m\]"
#    local BLUE="\[\033[0;34m\]"
#    local PURPLE="\[\033[0;35m\]"
#    local CYAN="\[\033[0;36m\]"
#    local WHITE="\[\033[0;37m\]"
#
#    local BBLACK="\[\033[1;30m\]"   #Bold colors
#    local BRED="\[\033[1;31m\]"
#    local BGREEN="\[\033[1;32m\]"
#    local BYELLOW="\[\033[1;33m\]"
#    local BBLUE="\[\033[1;34m\]"
#    local BPURPLE="\[\033[1;35m\]"
#    local BCYAN="\[\033[1;36m\]"
#    local BWHITE="\[\033[1;37m\]"
#
#    # return color to Terminal setting for text color
#    local RESET="\[\033[0m\]"
#
#    if [[ -z "$SSH_CLIENT" ]]; then  # Change host to yellow sshed to it
#        local SSHUSER=${CYAN}
#        local SSHHOST=${CYAN}
#    else
#        local SSHUSER=${YELLOW}
#        local SSHHOST=${YELLOW}
#    fi
#
#    if [[ "$UNAME" == "SunOS" ]]; then
#        if [[ $EUID == '0' ]]; then
#            export PS1="${CYAN}[${RED}\u@\h(`zonename`) \w${CYAN}] ${RED}"
#        else
#            export PS1="${CYAN}[\h(`zonename`) ${RED}\w${CYAN}] ${RESET}"
#        fi
#    else
#        if [[ $EUID == '0' ]]; then
#            export PS1="${WHITE}\A ${RED}\u${PURPLE}@${RED}\h${CYAN}[${WHITE}\!${CYAN}/\`if [[ \$? = "0" ]]; then echo "\\[\\033[35m\\]"; else echo "\\[\\033[31m\\]"; fi\`\j${CYAN}]:${WHITE}\w\n${RESET}\\$ "
#        else
#            export PS1="${WHITE}\A ${SSHUSER}\u${PURPLE}@${SSHHOST}\h${CYAN}[${WHITE}\!${CYAN}/\`if [[ \$? = "0" ]]; then echo "\\[\\033[35m\\]"; else echo "\\[\\033[31m\\]"; fi\`\j${CYAN}]:${WHITE}\w\n${RESET}\\$ "
#        fi
#    fi
#}


# Set the shell prompt, check if dumb term for Emacs TRAMP
#if [[ "$TERM" == "dumb" ]]; then
#  PS1='$ '
#else
#  #set_prompt
#
#  # Shell prompt from Linux Mint (from /etc/bash.basrhc)
#  if [[ ${EUID} == 0 ]] ; then
#    PS1='\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
#  else
#    PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '
#  fi
#fi

# New prompt with git branch (relies on git-prompt.sh
# https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh
# http://code-worrier.com/blog/git-branch-in-bash-prompt/

WHITE="\[\033[0;37m\]"
BBLACK="\[\033[1;30m\]"   #Bold colors
BRED="\[\033[1;31m\]"
BGREEN="\[\033[1;32m\]"
BYELLOW="\[\033[1;33m\]"
BBLUE="\[\033[1;34m\]"
BPURPLE="\[\033[1;35m\]"
BCYAN="\[\033[1;36m\]"
BWHITE="\[\033[1;37m\]"
RESET="\[\033[0m\]"

#source ~/.git-prompt.sh
#PS1="\[$GREEN\]\t\[$RED\]-\[$BLUE\]\u\[$YELLOW\]\[$YELLOW\]\w\[\033[m\]\[$MAGENTA\]\$(__git_ps1)\[$WHITE\]\$ "

# http://stackoverflow.com/questions/15883416/adding-git-branch-on-the-bash-command-prompt
#if [ "${SSH_CONNECTION}" ]; then
#  PS1="${RESET}[\u@${BRED}\h${RESET} \W${BGREEN}\$(__git_ps1 ' (%s)')${RESET}]\$ "
#else
#  PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
#fi

###
# shell prompt
###

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
#export PS1="\[\e[00;33m\]\u\[\e[0m\]\[\e[00;37m\]@\h \[\e[0m\]\[\e[00;36m\][\w]\[\e[0m\]\[\e[00;37m\] \`parse_git_branch\` \`\` \n\[\e[0m\]\[\e[00;31m\]\\$\[\e[0m\]\[\e[00;37m\] \[\e[0m\]"
#export PS1="${YELLOW}\u${RESET}${WHITE}@\h ${RESET}${CYAN}[\w]${RESET}${WHITE} \`parse_git_branch\` \`\` \n${RESET}${RED}\$${RESET}${WHITE} ${RESET}"
#PS1="${YELLOW}\u${RESET}${WHITE}@\h ${RESET}${CYAN}[\w]${RESET}${WHITE} \`parse_git_branch\`\n${WHITE}\$${RESET} "
# if CHIC02RR812G8WP change to thorn
if [[ ${HOSTNAME} == "CHIC02RR812G8WP.grubhub.local" ]];then
  PS1="${YELLOW}\u${RESET}${WHITE}@thorn ${RESET}${CYAN}[\w]${RESET}${WHITE} \`parse_git_branch\`\n${WHITE}\$${RESET} "
else
  PS1="${YELLOW}\u${RESET}${WHITE}@\h ${RESET}${CYAN}[\w]${RESET}${WHITE} \`parse_git_branch\`\n${WHITE}\$${RESET} "
fi
#\`if [[ \$? = "0" ]]; then echo '${RESET}\$'; else echo '${RED}\$${RESET}'; fi \` "
export PS1

####
# More Aliases
###

conncount() {
  netstat -an|grep ^tcp|grep -v LISTEN|awk '{print $6}'|sort|uniq -c|awk '{sum+=$1} {print $2 "=" $1} END {print "------------\nTOTAL=" sum}'
}

mcd() { mkdir -p "$1" && cd "$1"; }

alias speedtest='wget -O /dev/null http://speedtest.wdc01.softlayer.com/downloads/test100.zip'
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"

# Colored man pages
#export GROFF_NO_SGR=1
manc() {
    env LESS_TERMCAP_mb=$'\E[01;31m'   \
    LESS_TERMCAP_md=$'\E[01;38;5;74m'  \
    LESS_TERMCAP_me=$'\E[0m'           \
    LESS_TERMCAP_se=$'\E[0m'           \
    LESS_TERMCAP_so=$'\E[38;5;246m'    \
    LESS_TERMCAP_ue=$'\E[0m'           \
    LESS_TERMCAP_us=$'\E[04;38;5;146m' \
    man "$@"
}


#Get IPs for all network interfaces:
#iip() { ip a | grep "inet " | sed -e 's/^.*inet //g' -e 's/\/.*//g' | grep -v '127.0.0.1' }

alias gg="git grep"
alias git-unfuck="git reset --hard HEAD"

###
# Tmuxinator
###
[ -f ~/bin/tmuxinator.bash ] && source ~/bin/tmuxinator.bash
alias tml="tmux list-sessions"
alias tma="tmux -2 attach -t $1"
alias tmk="tmux kill-session -t $1"




#################
# Source workrc #
#################
if [ -e ${HOME}/.workrc ]; then
  source ~/.workrc
fi


###################
# System Specific #
###################
case "$UNAME" in
Darwin)  # Darwin Environment
if [[ ! -z $PS1 ]]; then echo ".Darwin bashrc loaded"; fi  # Interactive

TMPDIR=/tmp
# NOTE: PATH setup moved to bash_profile
EDITOR="${HOME}/bin/edit"
ALTERNATE_EDITOR="zile"
CLICOLOR=1
GROOVY_HOME=/usr/local/opt/groovy/libexec
export PATH MANPATH TMPDIR LD_LIBRARY_PATH CPPFLAGS TERM EDITOR ALTERNATE_EDITOR CLICOLOR GROOVY_HOME

if [[ $INSIDE_EMACS ]]; then
  echo "..Inside Emacs"
  TERM='vt100'
  #alias ls='ls --color=none'
  #alias grep='grep'
else
  TERM=xterm-256color
fi

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

function ediff {
    emacs --eval "(ediff \"$1\" \"$2\")"
}

function q { w3m -dump "http://google.com/search?q=$*" | more; }
function traffic { netstat -w1 -I"$@"; }
function qlook { qlmanage -p "$@" >& /dev/null & }
function mount_sshfs {
    if [ ! -d /tmp/$1 ]; then mkdir /tmp/$1; fi
    sshfs jacksond@$1: /tmp/$1 -ocache=no -onolocalcaches -ovolname=$1
}

if [[ -f ~/Library/LaunchAgents/gnu.emacs.daemon.plist ]]; then
    alias emacs_load="launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
    alias emacs_unload="launchctl unload -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist"
    alias emacs_status="launchctl list | grep emacs"
fi

if [[ -f ~/Library/mysql/com.mysql.mysqld.plist ]]; then
    alias start_mysql="sudo launchctl load ~/Library/mysql/com.mysql.mysqld.plist"
    alias stop_mysql="sudo launchctl unload ~/Library/mysql/com.mysql.mysqld.plist"
fi

###
# pyenv darwin
###
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

;; # end Darwin

SunOS)          # Based off of Solaris 10
if [[ ! -z $PS1 ]]; then echo ".Solaris bashrc loaded"; fi  # interactive

PATH=${HOME}/bin:/opt/perl/bin:/opt/SUNWspro/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/ucb:/usr/ccs/bin:/usr/openwin/bin:/usr/sfw/bin:/usr/sfw/sbin:/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin:/opt/csw/bin:/opt/csw/sbin
MANPATH=/usr/man:/usr/share/man:/opt/csw/man:/opt/csw/share/man:/usr/sfw/man:/usr/sfw/share/man:/opt/local/man:/usr/dt/man
LD_LIBRARY_PATH=/usr/local/lib:/opt/csw/lib:/usr/dt/lib:/usr/openwin/lib
TERM=vt100
export PATH MANPATH LD_LIBRARY_PATH TERM
;; # end SunOS

FreeBSD)
if [[ ! -z $PS1 ]]; then echo ".FreeBSD profile loaded"; fi  # interactive

PATH=${HOME}/bin:/bin:/usr/bin:/usr/local/bin:/usr/games:/usr/sbin
#TERM=vt100
TERM=xterm-256color
export PATH TERM
;; # end FreeBSD

Linux)  # Based off of Ubuntu
if [[ ! -z $PS1 ]]; then echo ".Linux bashrc loaded"; fi	# interactive

TERM=xterm-256color

# Debian / Ubuntu / Fedora / Other
# NOTE: PATH setup moved to bash_profile
#if [[ $(uname -a | grep Ubuntu) ]]; then
dist=`grep DISTRIB_ID /etc/*-release | awk -F '=' '{print $2}'`
if [[ "${dist}" == "Ubuntu" ]]; then
  echo "Ubuntu"
  LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30:ow=34:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:'
elif [[ -f /etc/fedora-release ]]; then
  echo "Fedora"
  LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=01;05;37;41:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=01;36:*.au=01;36:*.flac=01;36:*.mid=01;36:*.midi=01;36:*.mka=01;36:*.mp3=01;36:*.mpc=01;36:*.ogg=01;36:*.ra=01;36:*.wav=01;36:*.axa=01;36:*.oga=01;36:*.spx=01;36:*.xspf=01;36:'
  PS1='\[\033[0;34m\][$(date +%H:%M)] \[\033[0;32m\]\u\[\033[0;36m\]@\[\033[0;32m\]\h\[\033[0;34m\] \W\[\033[0;32m\]$(parse-git-branch.sh) \[\033[0;34m\]$\[\033[00m\] '
else # CentOS or Something Else [tm]
  echo "Not Ubuntu or Fedora"
  LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'
fi
GZIP="-9"
export MANPATH TERM LS_COLORS GZIP

## Open like command for Linux:  xdg-open or see
function open { xdg-open "$1" &> /dev/null & }

###
# Configure Emacs and Emacsclient
# adapted from http://philipweaver.blogspot.com/2009/08/emacs-23.html
###
alias ecw="emacsclient -n -c -a emacs" # start a windowed frame
alias ect="emacsclient -t -a emacs -nw" # start a terminal frame
alias ec="emacsclient -n -a emacs" # do not start a new frame
# export EDITOR="emacsclient -t"

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

###
# pyenv linux
###
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

;; # end Linux

*)
echo "uname not reporing Darwin, SunOS, FreeBSD, or Linux.  Where are we?"
;;

esac  # End System Specific case statement
