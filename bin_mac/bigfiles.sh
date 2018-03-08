#!/bin/bash
# Made by ioccatflashdancedotcx
# Version 1.0 - Fri Oct 31 19:27:31 CET 2003

echo
echo If you have many files it will take some time before _any_ output occurs.
echo The 20 biggest files in current dir and all subdirs:
echo
ls -lR | sort +4n > /tmp/bigfiles-tmp1
tail -20 /tmp/bigfiles-tmp1 | awk '{print $9}' > /tmp/bigfiles-tmp2
for i in `tail -20 /tmp/bigfiles-tmp2` ; do find . -name ${i} >> /tmp/bigfiles-tmp3 ; done
for i in `tail -20 /tmp/bigfiles-tmp3` ; do ls -l $i | sed s:\\./::g ; done
rm /tmp/bigfiles-tmp1
rm /tmp/bigfiles-tmp2
rm /tmp/bigfiles-tmp3
