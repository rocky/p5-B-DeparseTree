[![Build Status](https://travis-ci.org/rocky/p5-B-DeparseTree.png)](https://travis-ci.org/rocky/p5-B-DeparseTree)

SYNOPSIS
--------

Perl's B::Deparse but we save abstract tree information and associate
that with Perl text fragments.  These are fragments accessible by OP
address. With this, in Perl you can determine get exactly where you in
a program with granularity finer that at a line number boundary.

See [Exact Perl location with B::Deparse (and Devel::Callsite)](http://blogs.perl.org/users/rockyb/2015/11/exact-perl-location-with-bdeparse-and-develcallsite.html).

INSTALLATION
------------

To install this Devel::Trepan, run the following commands:

	perl Build.PL
	make
	make test
	[sudo] make install

LICENSE AND COPYRIGHT
---------------------

Copyright (C) 2015 Rocky Bernstein <rocky@cpan.org>
