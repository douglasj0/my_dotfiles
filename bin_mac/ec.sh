#!/bin/bash 

socket_file=$(lsof -c Emacs | grep server | tr -s " " | cut -d' ' -f8)

if [[ $socket_file == "" ]]; then        
    # Just run Emacs (with any arguments passed to the script)
    # It would be a good idea to parse the arguments and clean/remove
    # anything emacsclient specific. 
    # (ie. -e should be --eval for emacs)
    # note that emacsclient doesn't fix these args for you either
    # when using -a / --alternate-editor
    emacs $@ &

    # or on OSX
    #echo /Application/Emacs.app/Contents/MacOS/Emacs $@ &
else
    emacsclient $@ -n -s $socket_file
fi
