#!/usr/bin/perl -w
#correctBB.pl
#BoundingBox-Corrector by Max Kalus
 
use strict;
 
my @buffer;
my $bb;
my $found = 0;
 
#read from stdinput...
while(my $line = <>) {
  push(@buffer, $line);
  #search for PageBoundingBox
  if (!$found && $line =~ /^%%PageBoundingBox: (.*)$/) {
    $found = 1;
    $bb = $1;
  }
}
 
#now do output and change BoundingBox
foreach my $line (@buffer) {
  if ($line =~ /^%%BoundingBox:/) {
    print "%%BoundingBox: $bb\n";
  } else {
    print $line;
  }
}


