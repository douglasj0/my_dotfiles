#!/usr/bin/perl -w
# http://senojsitruc.blogspot.com/2005/12/drawing-names-for-christmas.html
use strict;

# this is how we generate the random seed value.
srand( time ^ $$ ^ unpack "%32L*", `ps wwaxl | gzip` );

# Obviously, the list of names.
my $list = [
 {name=>'Chris',    match=>-1},
 {name=>'Suzanne',  match=>-1},
 {name=>'Jill',     match=>-1},
 {name=>'Megan',    match=>-1},
 {name=>'Doug',     match=>-1},
 {name=>'Fran',     match=>-1}
];

# Each time a person's name is drawn, we put it in here,
# so that they are not drawn again.
my $done = [];

# for each person in the list, find a recipient for them.
for my $x (0..$#$list) {
  CJ: while ( 1 ) {
    my $r = int(rand(@{$list}));
    if( $done->[$r] || $r == $x ) {
      next CJ;
    }
    else {
      $list->[$x]{match} = $r;
      $done->[$r] = 1;
      last CJ;
    }
  }
}

for my $x (0..$#$list) {
  print $list->[$x]{name} . " drew " .
        $list->[$list->[$x]{match}]{name} . "\n";
}
