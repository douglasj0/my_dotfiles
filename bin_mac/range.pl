#!/usr/bin/perl

if (@ARGV < 2) {
        print "usage: range start end [step]\n";
        exit 2;
}

$i = $ARGV[0];

$step = 1;
if (@ARGV == 3) {
        $step = $ARGV[2];
}

for ($i = $ARGV[0]; $i <= $ARGV[1]; $i += $step) {
        print "$i\n";
}
