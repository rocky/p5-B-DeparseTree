#!/usr/bin/env perl
use rlib '.';
use helper;
use strict;
use English;

use feature (sprintf(":%vd", $^V)); # to avoid relying on the feature
                                    # logic to add CORE::

test_ops('unops.pm');
done_testing();
