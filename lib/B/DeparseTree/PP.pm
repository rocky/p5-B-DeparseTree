# Common PP opcodes. Specifc Perl versions can override these.

# Copyright (c) 2015 Rocky Bernstein
#
use strict;
use warnings ();

use rlib '../..';

package B::DeparseTree::PP;

use B::DeparseTree::Common;

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.0.0';

@ISA = qw(Exporter);
@EXPORT = qw(pp_leave pp_lineseq pp_scope);

sub pp_scope { scopeop(0, @_); }
sub pp_lineseq { scopeop(0, @_); }
sub pp_leave { scopeop(1, @_); }


1;
