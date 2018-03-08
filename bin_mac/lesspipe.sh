#!/bin/sh
# This is a preprocessor for 'less'.  It is used when this environment
# variable is set:   LESSOPEN="|lesspipe.sh %s"

lesspipe() {
  case "$1" in
  *.tar) gtar tvvf $1 2>/dev/null ;; # View contents of .tar and .tgz files
  *.tgz) gtar tzvvf $1 2>/dev/null ;;
  *.tar.gz) gtar tzvvf $1 2>/dev/null ;;
  *.tar.Z) gtar tzvvf $1 2>/dev/null ;;
  *.tar.z) gtar tzvvf $1 2>/dev/null ;;
  *.Z) gzip -dc $1  2>/dev/null ;; # View compressed files correctly
  *.z) gzip -dc $1  2>/dev/null ;;
  *.gz) gzip -dc $1  2>/dev/null ;;
  *.zip) unzip -l $1 2>/dev/null ;;
  *.1|*.2|*.3|*.4|*.5|*.6|*.7|*.8|*.9|*.n|*.man) FILE=`file -L $1` ; # groff src
    FILE=`echo $FILE | cut -d ' ' -f 2`
    if [ "$FILE" = "troff" ]; then
      groff -s -p -t -e -Tascii -mandoc $1
    fi ;;
#  *) FILE=`file -L $1` ; # Check to see if binary, if so -- view with 'strings'
#    FILE1=`echo $FILE | cut -d ' ' -f 2`
#    FILE2=`echo $FILE | cut -d ' ' -f 3`
#    if [ "$FILE1" = "Linux/i386" -o "$FILE2" = "Linux/i386" \
#         -o "$FILE1" = "ELF" -o "$FILE2" = "ELF" ]; then
#      strings $1
#    fi ;;
  esac
}

lesspipe $1
