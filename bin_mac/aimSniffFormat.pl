#!/usr/bin/perl
# aimsniff formatter.
# Eric Herrera, <me at ericherrera dot com>

=pod

=head1 NAME

aimSniff Formatter

=head1 USAGE

Just pipe the data in/out.

=head1 DESCRIPTION

I wrote this using output from AimSniff 0.9d

This will format the output from aimSniff.pl into readable lines of
text. It will also remove the double message entries. It is assumed
that the messages read will all be from the same user. So it will
prepend the other party's handle at the beginning of each line(message).
You can then grep for ^handle to see all messages to/from that user.

=head1 EXAMPLES

  aimSniff.pl --nodb |perl aimSniffFormat.pl > outfile

  cat aimsniff_file |perl aimSniffFormat.pl

=head1 SAMPLE MESSAGE FORMAT

AIM
        Type: Outgoing Message
        Timestamp: 2004-7-2 10:0:5
        FROM: foobaruser
        FAMILY: 00040006
        MESSAGE: how is the foo today?
        SRCIP: ---.---.---.---
        DESTHANDLE: barfoouser



=cut

while (<>){
	if (/^AIM\n/){
		my %data = &get_params;
		next if $data{MESSAGE} eq $last{MESSAGE};
		my $op = '';
		if ($data{Type} eq "Incoming Message"){
			$op = $data{FROM};
		}
		elsif ($data{Type} eq "Outgoing Message"){
			$op = $data{DESTHANDLE};
		}
		else { $op = $data{Type}; }
		print "$op $data{Timestamp} $data{FROM}: $data{MESSAGE}\n";
		%last = %data;
	}
}

sub get_params {
	my %tmp = ();
	while (<>){
		last if /^\n/;
		my ($name, $value) = /(\w*): (.*)$/;
		$tmp{$name} = $value;
	}
	return %tmp;
}
