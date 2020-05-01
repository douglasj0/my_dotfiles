# -*- mode: sh -*-
# `.zlogin' is sourced in login shells. It should contain commands that should be executed only in login shells

# clear
# cd
# stty dec new cr0 -tabs
# stty new cr0 -tabs
stty cr0 -tabs
ttyctl -f  # freeze the terminal modes... can't change without a ttyctl -u
umask 022
MAILCHECK=60
mesg y
uptime
unlimit
