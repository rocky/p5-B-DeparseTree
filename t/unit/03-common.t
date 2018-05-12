#!/usr/bin/env perl
use strict;
use warnings;
use rlib '../../lib';

use B qw(main_root);

use Test::More;
note( "Testing B::DeparseTree B::DeparseTree:Common" );

BEGIN {
use_ok( 'B::DeparseTree::Common' );
}

my $deparse = B::DeparseTree::Common->new();

Test::More::note ( "info_from_list() testing" );
my @texts = ('a', 'b', 'c');
my $info = info_from_list(main_root, $deparse, \@texts, ', ', 'test1', {});
is $info->{text}, 'a, b, c';
is $info->{type}, 'test1';

@texts = (['d', 1], ['e', 2], 'f');
my $info1 = info_from_list(main_root, $deparse, \@texts, '', 'test2', {});
is $info1->{text}, 'def';

my $info2 = info_from_list(main_root, $deparse, [$info, $info1], ':', 'test3', {});
is $info2->{text}, 'a, b, c:def';

my $info3 = info_from_text(main_root, $deparse, 'foo', 'test4', {});
is $info3->{text}, 'foo';


Test::More::done_testing();
