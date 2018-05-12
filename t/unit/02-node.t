#!/usr/bin/env perl
use strict;
use warnings;
use rlib '../../lib';
use blib;

use Test::More;
note( "Testing B::DeparseTree B::DeparseTree::Node" );

BEGIN {
use_ok( 'B::DeparseTree::Node' );
}

package B::DeparseTree::NodeTest;
sub new($) {
    my ($class) = @_;
    bless {}, $class;
}
sub combine2str($$$) {
    my ($self, $sep, $texts) = @_;
    join($sep, @$texts);
}

my $deparse = __PACKAGE__->new();
my $node = B::DeparseTree::Node->new('op', $deparse, ['X'], 'test', {});
Test::More::cmp_ok $node->{'text'}, 'eq', 'X';

Test::More::done_testing();
