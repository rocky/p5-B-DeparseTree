#!/usr/bin/env perl
use rlib '.';
use helper;
use Data::Dumper;
use B::DeparseTree::Fragment;  # for dump
use strict;
use English;

BEGIN {
    plan skip_all => 'All indirops need fixing';
}

use feature (sprintf(":%vd", $^V)); # to avoid relying on the feature
                                    # logic to add CORE::

test_ops('indirops.pm');
done_testing();
