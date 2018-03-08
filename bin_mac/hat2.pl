#!/usr/bin/perl

my @names = qw(Chris Suzanne Jill Megan Doug Fran);
my @deck = @names;		# deck of cards to draw from

for my $person (@names)		# pick an entry from deck at random:
{
  my $draw = splice @deck, rand @deck, 1;
  if ($draw eq $person) 	# not to self!
  {
    push @deck, $draw; 		# put it back in deck
    redo;			# this is where it can get in infinite loop
  }
print "$person drew $draw\n";
}
