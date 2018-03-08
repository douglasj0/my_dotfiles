#!/bin/sh

for i in `cat $@` ;
do
  PRODID=`echo ${i} | awk -F, '{print $1}'`
  TITLE=`echo ${i} | awk -F, '{print $2}'`
  wget http://docs-pdf.sun.com/${PRODID}/${PRODID}.pdf
  mv ${PRODID}.pdf ${TITLE}_\(${PRODID}\).pdf
done
