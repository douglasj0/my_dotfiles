# -*-perl-*-
eval 'exec perl -w -S $0 ${1+"$@"}'
if 0;

use warnings;
use strict;
use ExtUtils::Installed;

# Find all the installed modules
print("Finding all installed Perl modules...\n");

my $installed = ExtUtils::Installed->new();
my $max_length = 0;

# Let's find the longest name
foreach my $module (grep(!/^Perl$/, $installed->modules()))
{
(length ($module) > $max_length) and $max_length = length ($module);

}

# A nice way to print the results
my $format = "    %-".$max_length."s  Version %s\n";

# We display all the modules along with their version
foreach my $module (grep(!/^Perl$/, $installed->modules()))
{
my $version = $installed->version($module) || "???";

printf($format, $module, $version);
}
