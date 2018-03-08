#!/bin/sh

echo "Enter the filename..."
read _name
for f in $*; do
    if [ -f $_name ]; then
        uuencode $f $f >> $_name
        echo $f
     else
        uuencode $f $f > $_name
        echo $f
    fi
done
