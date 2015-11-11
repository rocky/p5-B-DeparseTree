B::ParseTree -- Deparse command for Devel::Trepan
====================================================================

SYNOPSIS
--------

Perl's B::Deparse but we save abstract tree information and associate
that with Perl text fragments.  These are fragments accessible by OP
address. With this, in Perl you can determine get exactly where you in
a program with granularity finer that at a line number boundary.

See [Exact Perl location with B::Deparse (and Devel::Callsite](http://blogs.perl.org/users/rockyb/2015/11/exact-perl-location-with-bdeparse-and-develcallsite.html).

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

This program is distributed WITHOUT ANY WARRANTY, including but not
limited to the implied warranties of merchantability or fitness for a
particular purpose.

The program is free software. You may distribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation (either version 2 or any later version) and
the Perl Artistic License as published by O’Reilly Media, Inc. Please
open the files named gpl-2.0.txt and Artistic for a copy of these
licenses.
