#!/usr/bin/perl

use File::Basename;

my $mode = shift @ARGV;
defined($mode) or usage();

&$mode(@ARGV);
exit;

sub usage {
   my $script = basename($0);
   while (my $line = <DATA>) {
      eval( "\$line = \"$line\"" );
      print $line;
   }
   exit 1;
}

sub add {
   ( @_ == 2 ) or usage();
   my ( $name, $path ) = @_;
   my $sharepoint = "/config/SharePoints/$name";
   share_exists($name) and die "Share with name '$name' already exists.\n";
   system("niutil -create / '$sharepoint'");
   $? and die "Couldn't create share '$name'\n";
   system("niutil -createprop / '$sharepoint' directory_path '$path'");
   $? and die "Couldn't set path '$path' for share '$name'\n";
}

sub rm {
   ( @_ == 1 ) or usage();
   my ( $name ) = @_;
   my $sharepoint = "/config/SharePoints/$name";
   share_exists($name) or die "No such share: '$name'\n";
   system("niutil -destroy / '$sharepoint'");
   $? and die "Couldn't remove share '$name': $!\n";
}

sub list {
   my %inames;
   @inames{@_} = ();
   open( LIST, "niutil -list / /config/SharePoints|" )
      or die "Couldn't list shares: $!\n";
   while ( <LIST> ) {
      my $name = ( split /\s+/, $_ )[1];
      ( !@_ or exists($inames{$name}) ) and push @names, $name;
   }
   close LIST;
   foreach my $name ( @names ) {
      my $path = `niutil -readprop / '/config/SharePoints/$name' directory_path`;
      print "$name -> $path";
   }
}

sub start {
   my $pid = get_pid();
   defined( $pid ) and die "File server is already running with pid $pid.\n";
   system("/usr/sbin/AppleFileServer &");
}

sub stop {
   my $pid = get_pid();
   defined($pid) or return;
   kill 15, $pid;
   sleep 1;
   kill( 0, $pid ) and die "process $pid did not die on signal 15\n";
}

sub restart {
   stop();
   start();
}

sub share_exists {
   my ( $name ) = @_;
   my $sharepoint = "/config/SharePoints/$name";
   system("niutil -list / '$sharepoint' >/dev/null 2>&1");
   return ! $?;
}

sub get_pid {
   my $pid;
   open PS, "ps -ax|";
   while (<PS>) {
      m/AppleFileServer\s*$/ or next;
      ( $pid ) = m/^\s*(\d+)/;
      last;
   }
   close PS;
   return $pid;
}

__DATA__
$script - Control apple file sharing.

Usage:
   $script add <name> <path>     - share directory <path> as <name>
   $script rm <name>             - remove share named <name>
   $script list [<name> ...]     - list shares
   $script start                 - turn on file sharing
   $script stop                  - turn off file sharing
   $script restart               - restart file sharing (necessary for add or
                                   rm to take effect)

You must be root for all commands except 'list'.

