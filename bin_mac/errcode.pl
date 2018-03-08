#!/usr/bin/perl
###
# Searches a list of error codes in the
# Carbon CoreServices framework header files
#
# this script supports negative error codes,
# filters out all the un-asked-for codes, and
# formats the whole thing prettily. It also
# supports multiple error codes
#
# written by discordantus at www.macosxhints.com
# 6/7/2003
###
use warnings;
use strict;

# variables:
my ($path,$file,$name,$desc,$errnum);

# set the path to the header file
$path = '/System/Library/Frameworks/'
.'CoreServices.framework/Versions/Current/Frameworks/'
.'CarbonCore.framework/Versions/Current/Headers/MacErrors.h';

# open it and search for each argument.
while($errnum=shift){
    open $file, '<', $path;
    while(<$file>){
        if (m/=\s$errnum\,/) {
            $name=$_;
            $name=~s/^\s*(\S*)\s.*$/$1/;
            chomp $name;
            $desc=$_;
            $desc=~s:^.*?/\*(.*)\*/:$1:;
            chomp $desc;
            print "$name ($errnum): $desc", "\n";
        }
    }
    close $file;
}

exit 0;
