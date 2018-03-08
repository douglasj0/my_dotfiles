#!/usr/bin/perl -w

# Script to split up XChat log files by date by: Alan Cameron
#
# Purpose: XChat stores it's log files for /msg and DCC chat's as
#          nick.xchatlog in it's log file dir.  Being the kind of
#          person I am, I like to keep log files of my conversations
#          but some people use multiple nicks so the conversations
#          end up in different files.  I wanted to have all the
#          conversations in one file, so I wrote this script to
#          split the log files up by the date of the conversation.
#          This creates a bunch of files of the form:
#          foo1042353175.log foo1042354456.log foo1042355691.log
#          one for each conversation in the original log file
#          where foo is the prefix you give it, and the number is
#          the epoch time of when the conversation took place.
#          These can be combined to form one log file with all
#          the conversations in chronological order.

use strict;
use Time::Local;
sub debug ($);

die "usage: $0 <prefix> <logfile(s)>\n" unless @ARGV;

# Set DEBUG to 1 to see progress
my $DEBUG = 0;

# Prefix for time stamped logfiles
my $prefix = shift;

# Files to be processed
my @files = @ARGV;

# Convert month names to numbers
my %months;
@months{qw/Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec/} = (0 .. 11);

# For each file, read through the file, and when you find a new
# section, close the current file, and open a new one with the name
# prefix<epoch time>.log  where <epoch time> is the timestamp for
# when that conversation started.
#
# If the line doesn't start a new section, just print it to the
# current sections file.
#
# NOTE: Assumes XChat Log files always have a BEGIN LOGGING line
# as the very first line of the file.  If this behavoir of XChat changes
# The script will have to be changed.
for my $file (@files) {
    open FILE, "$file" or die "Can't open $file: $!\n";
    debug "Opened $file";
    while (<FILE>) {
        if (/^\*\*\*\* BEGIN LOGGING AT (.*)/) {
            close OUT;
            my ($mon, $day, $hr, $min, $sec, $yr) =
              $1 =~ /\w+\s+(\w+)\s+(\d+)\s+(\d+):(\d+):(\d+)\s+(\d+)/;
            $mon = $months{$mon};
            my $time = sprintf "%010d",
              timelocal($sec, $min, $hr, $day, $mon, $yr);
            open OUT, ">$prefix$time.log"
              or die "Can't open $prefix$time.log: $!\n";
            debug "Opened $prefix$time.log";
        }
        print OUT $_;
    }
}

sub debug ($) {
    print STDERR $_[0], "\n" if $DEBUG;
}
