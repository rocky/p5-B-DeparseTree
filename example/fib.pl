#!/usr/bin/env perl
use rlib '../lib';

use B::DeparseTree;
use B::Deparse;
use Data::Printer;
use B::Concise;

sub fib($) {
    my $x = shift;
    return 1 if $x <= 1;
    fib($x-1) + fib($x-2);
}

printf "fib(2)= %d, fib(3) = %d, fib(4) = %d\n", fib(2), fib(3), fib(4);

# my $walker = B::Concise::compile('-basic', '-src', 'fib', \&fib);
# B::Concise::set_style_standard('debug');
# B::Concise::walk_output(\my $buf);
# $walker->();			# walks and renders into $buf;
# print($buf);

my $deparse = B::DeparseTree->new("-p", "-l", "-c", "-sC");

my @exprs = $deparse->coderef2list(\&fib);
import Data::Printer colored => 0;
Data::Printer::p(@exprs);
print "\n", '-' x 30, "\n";
print $deparse->coderef2text(\&fib);
print "\n", '=' x 50, "\n";

my $deparse_old = B::Deparse->new("-p", "-l", "-sC");
print $deparse_old->coderef2text(\&fib);
