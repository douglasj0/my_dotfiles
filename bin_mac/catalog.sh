i=`df -k | grep Music | awk '{ print $6 }'`
find $i -type d -print | sed s+/Volumes/++g | sort >> music.txt
echo "####################" >> music.txt
