#$/bin/sh

for i in `jot 255 1`
do
  echo checing 192.168.1.$i
  ping -c 1 -t 1 192.168.1.$i > /dev/null
done
arp -a
