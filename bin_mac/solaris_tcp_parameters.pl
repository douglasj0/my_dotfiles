#!/usr/bin/perl
# this script gets all the tcp/udp tunable values

use strict;

#read the possible tcp/udp values into an array called tcpip
my @tcpip=`ndd -get /dev/tcp \\?; ndd -get /dev/udp \\? `;

#process the array
foreach (@tcpip)
{
# break down each line of the array into the variables we require
(my $parameter, my $junk)= split(/\(/, $_);
(my $parameter, my $junk2)=split(/ /, $parameter);
chomp ($junk);
chomp ($parameter);

#there are some values for tcp or udp that we do not wish to get
#these are removed fron the processing loop here
if ( $junk ne "read only)" && $junk ne "write only)" && $parameter ne "tcp_host_
param" && $parameter ne "?" )
{
        #get the tcp and udp values and display them
        (my $type, my $junk)= split(/_/, $parameter);
        my $result=`ndd /dev/$type $parameter`;
        chomp ($result);
        print "$parameter\t";
        print "$result\n";
}
}