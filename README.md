[![Build Status](https://travis-ci.org/rocky/p5-B-DeparseTree.png)](https://travis-ci.org/rocky/p5-B-DeparseTree)

SYNOPSIS
--------

Perl's B::Deparse but we save abstract tree information and associate
that with Perl text fragments.  These are fragments accessible by OP
address. With this, you can determine get exactly where you inside Perl in
a program with granularity finer that at a line number boundary.

Uses for this could be in stack trace routines like _Carp_. It is used
in the [deparse](https://metacpan.org/pod/Devel::Trepan::Deparse)
command extension to
[Devel::Trepan](https://metacpan.org/pod/Devel::Trepan).

See [Exact Perl location with B::Deparse (and Devel::Callsite)](http://blogs.perl.org/users/rockyb/2015/11/exact-perl-location-with-bdeparse-and-develcallsite.html).

EXAMPLE
-------

   use B::DeparseTree;
   my $deparse = B::DeparseTree->new();

   # create a subroutine to deparse...
   sub my_abs($) {
       return $a < 0 ? -$a : $a;
   };

   my $deparse_tree = B::DeparseTree->new();
   my $tree_node = $deparse_tree->coderef2info(\&my_abs);
   print $tree_node->{text};

The above produces:

    ($)
    {
        return $a < 0 ? -$a : $a
    }

but the result are reconstructed purely from the OPnode tree. To show
parent-child information in the tree:

    use B::DeparseTree::Fragment;
    B::DeparseTree::Fragment::dump_relations($deparse_tree);

which produces:

    0: ==================================================
    Child info:
        addr: 0x23065a0, parent: 0x23d9720
        op: negate
        text: -$a

    return $a < 0 ? -$a : $a
                    ---
    0: ==================================================
    1: ==================================================
    Child info:
        addr: 0x23065f0, parent: 0x23065a0
        op: gvsv
        text: $a

    return $a < 0 ? -$a : $a
                     --
    1: ==================================================
    2: ==================================================
    Child info:
        addr: 0x2306630, parent: 0x23065a0
        op: gvsv
        text: $a

    return $a < 0 ? -$a : $a
                     --
    2: ==================================================
    3: ==================================================
    Child info:
        addr: 0x2306670, parent: 0x23d9720
        op: lt
        text: $a < 0

    return $a < 0 ? -$a : $a
           ------
    3: ==================================================
    4: ==================================================
    Child info:
        addr: 0x23066b8, parent: 0x2306670
        op: B::IV=SCALAR(0x2fdb5b8)
        text: 0

    return $a < 0 ? -$a : $a
                -
    4: ==================================================
    5: ==================================================
    Child info:
        addr: 0x23066f8, parent: 0x2306670
        op: gvsv
        text: $a

    return $a < 0 ? -$a : $a
           --
    5: ==================================================
    6: ==================================================
    Child info:
        addr: 0x2306738, parent: 0x2306670
        op: gvsv
        text: $a

     return $a < 0 ? -$a : $a
            --
    6: ==================================================
    9: ==================================================
    Child info:
        addr: 0x23d9600, parent: 0x23d9578
        op: nextstate
        text:

    return $a < 0 ? -$a : $a

    9: ==================================================
    10: ==================================================
    Child info:
        addr: 0x23d9660, parent: 0x23d9698
        op: pushmark
        text: return $a < 0 ? -$a : $a

    return $a < 0 ? -$a : $a
    ||||||
    10: ==================================================
    11: ==================================================
    Child info:
        addr: 0x23d9698, parent: 0x23d9578
        op: return
        text: return $a < 0 ? -$a : $a

    return $a < 0 ? -$a : $a
    ------------------------
    11: ==================================================
    12: ==================================================
    Child info:
        addr: 0x23d96e0, parent: 0x23d9698
        op: cond_expr
        text: $a < 0 ? -$a : $a

    return $a < 0 ? -$a : $a
           -----------------
    12: ==================================================
    ....


INSTALLATION
------------

Currently we only support Perl 5.18, 5.20, 5.22, 5.24 and 5.26.

To install this Devel::Trepan, run the following commands:

	perl Build.PL
	make
	make test
	[sudo] make install

LICENSE AND COPYRIGHT
---------------------

Copyright (C) 2015, 2017, 2018 Rocky Bernstein <rocky@cpan.org>
