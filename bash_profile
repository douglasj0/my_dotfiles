# -*- shell-script -*-
# ~/.bash_profile - executed for login shells
#
# ------------------------------------------------------------------------------
echo ".bash_profile loaded"

# Source .profile and .bashrc if they're readable
[[ -r ~/.profile ]] && source ~/.profile # PATH setup
[[ -r ~/.bashrc ]] && source ~/.bashrc   # Bash aliases and prompt
