#!/bin/bash

server=$1

/usr/bin/ssh -o PasswordAuthentication=no -o PubkeyAuthentication=yes -o PreferredAuthentications=publickey -o ConnectTimeout=1 ${server}

RC=$?

if [[ $RC == "255" ]];then
    /usr/bin/ssh ${server} "mkdir -p ${HOME}/.ssh && chmod 700 ${HOME}/.ssh ; echo $(cat ~/.ssh/authorized_keys) >> ${HOME}/.ssh/authorized_keys ; chmod 644 ${HOME}/.ssh/authorized_keys"
    sleep 2
    /usr/bin/ssh -o PasswordAuthentication=no -o PubkeyAuthentication=yes -o PreferredAuthentications=publickey -o ConnectTimeout=2 ${server}
fi
