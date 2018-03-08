#!/bin/sh
echo "Enter username:"
read new_username
echo "Enter the full name for user $new_username's:"
read long_name
echo "Will $new_username an Admin user (y/N)?"
read godlike_powers
new_uid=`nidump passwd . | awk -F: '{print $3f}' | sort -n|tail -1`
new_uid=`expr $new_uid + 1`
nicl . -create /users/$new_username
nicl . -create /users/$new_username uid $new_uid
nicl . -create /users/$new_username realname "$long_name"
nicl . -create /users/$new_username passwd ""
nicl . -create /users/$new_username gid 20
nicl . -create /users/$new_username shell "/bin/tcsh"
nicl . -create /users/$new_username home "/Users/$new_username"
nicl . -create /users/$new_username _writers_passwd $new_username
passwd $new_username
ditto /System/Library/UserTemplate/English.lproj /Users/$new_username
chown -R $new_username:staff /Users/$new_username
nicl . -read /users/$new_username
if [ "$godlike_powers" = Y -o "$godlike_powers" = y ]
then
nicl . -append /groups/wheel users $new_username
nicl . -append /groups/admin users $new_username
nicl . -read /groups/wheel
nicl . -read /groups/admin
fi
