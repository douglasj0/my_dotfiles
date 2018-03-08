#!/bin/sh
sqlite3 -separator '    ' ~/Library/Application\ Support/AddressBook/AddressBook-v22.abcddb "select e.ZADDRESSNORMALIZED,p.ZFIRSTNAME,p.ZLASTNAME,p.ZORGANIZATION from ZABCDRECORD as p,ZABCDEMAILADDRESS as e WHERE e.ZOWNER = p.Z_PK;" | grep $1
