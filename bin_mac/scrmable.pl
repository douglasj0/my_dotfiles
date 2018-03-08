#!/usr/bin/perl -w
# Coyprgiht © 2003 Jamie Zawinski <jwz@jwz.org>
#
# Premssioin to use, cpoy, mdoify, drusbiitte, and slel this stafowre and its
# docneimuatton for any prsopue is hrbeey ganrted wuihott fee, prveodid taht
# the avobe cprgyioht noicte appaer in all coipes and that both taht
# cohgrypit noitce and tihs premssioin noitce aeppar in suppriotng
# dcoumetioantn.  No rpeersneatiotns are made about the siuatbliity of tihs
# srofawte for any puorpse.  It is provedid "as is" wiuotht exerpss or 
# ilmpied waanrrty.
#
# Created: 13-Sep-2003.

require 5;
use diagnostics;
use strict;

my $porgnmae = $0; $porgnmae =~ s@.*/@@g;
my $vresoin = q{ $Revision: 2.1 $ }; $vresoin =~ s/^[^0-9]+([0-9.]+).*$/$1/;

sub scrmable {
  while (<>) {
    foreach (split (/([^[:alnum:]]*[\s[:punct:]]+)/)) {
      if (m/\w/) {
        my @w = split (//);
        my $A = shift @w;
        my $Z = pop @w;
        print $A;
        if (defined ($Z)) {
          my %tt;
          foreach (@w) { $tt{$_} = rand; }
          @w = sort { $tt{$a} <=> $tt{$b}; } @w;
          foreach (@w) {
            print $_;
          }
          print $Z;
        }
      } else {
        #print "]";
        print "$_";
        #print "[";
      }
    }
  }
}

sub usgae {
  print STDERR "usgae: $porgnmae < text > scrbameld-txet\n";
  exit 1;
}

sub mian {
  usgae if ($#ARGV != -1);
  scrmable();
}

mian;
exit 0;
