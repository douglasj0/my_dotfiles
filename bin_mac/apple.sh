#!/bin/zsh
#fclear
echo
uptime
echo

#-----------------------
# Greeting, motd etc...
#-----------------------

# Define some colors first:
red='\e[0;31m'
green='\e[0;32m'
orange='\e[0;33m'
blue='\e[0;34m'
purple='\e[0;35m'
cyan='\e[0;36m'
grey='\e[0;37m'
darkgrey='\e[0;90m'
lightred='\e[0;91m'
lightgreen='\e[0;92m'
yellow='\e[0;93m'
lightblue='\e[0;94m'
lightpurple='\e[0;95m'
turquoise='\e[0;96m'
NC='\e[0m'              # No Color

# Get versions
HOST_NAME=`uname -n`
KERNEL_VER=`uname -r`
OS_NAME=`uname -s`
OS_VER=`sw_vers -productVersion`

# Looks best on a black background.....

echo -e  "${green}          .:'"
echo -e  "${green}       __ :'__             ${grey}Welcome to ${red}${HOST_NAME}${grey}!"
echo -e  "${green}    .'\`__\`-'__\`\`."
echo -e "${yellow}   :__________.-'"
echo -e    "${red}   :_________:             ${grey}Running on ${red}${OS_NAME}"
echo -e "${purple}    :_________\`-;      ${grey}Kernel Version ${red}${KERNEL_VER}"
echo -e   "${cyan}     \`.__.-.__.'             ${grey}Mac OS X ${grey}${red}${OS_VER}${NC}"
echo
