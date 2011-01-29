#!/usr/bin/perl

use strict;
use warnings;

my $padsize = (4096 - ((-s $ARGV[0]) % 4096)) - 4;

open(EXE, '>>', $ARGV[0]);
for (my $i = 0; $i < $padsize; $i++) {
    syswrite EXE, "\0", 1;
}
print EXE pack("H*", "efbeadde");
