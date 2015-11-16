package B::DeparseTree;

use rlib '.';
use strict;
use vars qw(@ISA $VERSION);

$VERSION = '2.0.0';

my $module;
if ($] >= 5.020 and $] < 5.022) {
    $module = "P520";
    require "B/DeparseTree/${module}.pm";
    *compile = \&B::DeparseTree::P520::compile;
} elsif ($] >= 5.022) {
    $module = "P522";
    require "B/DeparseTree/${module}.pm";
    *compile = \&B::DeparseTree::P522::compile;
} else {
    die "Can only handle Perl 5.20 and 5.22";
}

@ISA = ("B::DeparseTree::$module");

=head1 NAME

B::DeparseTree - Perl compiler backend to produce perl code and save fragments as an abstract tree

=head1 SYNOPSIS

B<perl> B<-MO=DeparseTree>[B<,-d>][B<,-f>I<FILE>][B<,-p>][B<,-q>][B<,-l>]
        [B<,-s>I<LETTERS>][B<,-x>I<LEVEL>] I<prog.pl>

=head1 DESCRIPTION

B::DeparseTree is a backend module for the Perl compiler that generates
perl source code, based on the internal compiled structure that perl
itself creates after parsing a program.  The output of B::DeparseTree won't
be exactly the same as the original source, since perl doesn't keep
track of comments or whitespace, and there isn't a one-to-one
correspondence between perl's syntactical constructions and their
compiled form, but it will often be close.  When you use the B<-p>
option, the output also includes parentheses even when they are not
required by precedence, which can make it easy to see if perl is
parsing your expressions the way you intended.

While B::DeparseTree goes to some lengths to try to figure out what your
original program was doing, some parts of the language can still trip
it up; it still fails even on some parts of Perl's own test suite.  If
you encounter a failure other than the most common ones described in
the BUGS section below, you can help contribute to B::DeparseTree's
ongoing development by submitting a bug report with a small
example.

=head1 OPTIONS

As with all compiler backend options, these must follow directly after
the '-MO=DeparseTree', separated by a comma but not any white space.

=over 4

=item B<-d>

Output data values (when they appear as constants) using Data::Dumper.
Without this option, B::DeparseTree will use some simple routines of its
own for the same purpose.  Currently, Data::Dumper is better for some
kinds of data (such as complex structures with sharing and
self-reference) while the built-in routines are better for others
(such as odd floating-point values).

=item B<-f>I<FILE>

Normally, B::DeparseTree deparses the main code of a program, and all the subs
defined in the same file.  To include subs defined in
other files, pass the B<-f> option with the filename.
You can pass the B<-f> option several times, to
include more than one secondary file.  (Most of the time you don't want to
use it at all.)  You can also use this option to include subs which are
defined in the scope of a B<#line> directive with two parameters.

=item B<-c>

Add COP address locations to the '# line' declarations

=item B<-l>

Add '# line' declarations to the output based on the line and file
locations of the original code.

=item B<-p>

Print extra parentheses.  Without this option, B::DeparseTree includes
parentheses in its output only when they are needed, based on the
structure of your program.  With B<-p>, it uses parentheses (almost)
whenever they would be legal.  This can be useful if you are used to
LISP, or if you want to see how perl parses your input.  If you say

    if ($var & 0x7f == 65) {print "Gimme an A!"}
    print ($which ? $a : $b), "\n";
    $name = $ENV{USER} or "Bob";

C<B::DeparseTree,-p> will print

    if (($var & 0)) {
        print('Gimme an A!')
    };
    (print(($which ? $a : $b)), '???');
    (($name = $ENV{'USER'}) or '???')

which probably isn't what you intended (the C<'???'> is a sign that
perl optimized away a constant value).

=item B<-P>

Disable prototype checking.  With this option, all function calls are
deparsed as if no prototype was defined for them.  In other words,

    perl -MO=DeparseTree,-P -e 'sub foo (\@) { 1 } foo @x'

will print

    sub foo (\@) {
	1;
    }
    &foo(\@x);

making clear how the parameters are actually passed to C<foo>.

=item B<-q>

Expand double-quoted strings into the corresponding combinations of
concatenation, uc, ucfirst, lc, lcfirst, quotemeta, and join.  For
instance, print

    print "Hello, $world, @ladies, \u$gentlemen\E, \u\L$me!";

as

    print 'Hello, ' . $world . ', ' . join($", @ladies) . ', '
          . ucfirst($gentlemen) . ', ' . ucfirst(lc $me . '!');

Note that the expanded form represents the way perl handles such
constructions internally -- this option actually turns off the reverse
translation that B::DeparseTree usually does.  On the other hand, note that
C<$x = "$y"> is not the same as C<$x = $y>: the former makes the value
of $y into a string before doing the assignment.

=item B<-s>I<LETTERS>

Tweak the style of B::DeparseTree's output.  The letters should follow
directly after the 's', with no space or punctuation.  The following
options are available:

=over 4

=item B<v>I<STRING>B<.>

Print I<STRING> for the value of a constant that can't be determined
because it was optimized away (mnemonic: this happens when a constant
is used in B<v>oid context).  The end of the string is marked by a period.
The string should be a valid perl expression, generally a constant.
Note that unless it's a number, it probably needs to be quoted, and on
a command line quotes need to be protected from the shell.  Some
conventional values include 0, 1, 42, '', 'foo', and
'Useless use of constant omitted' (which may need to be
B<-sv"'Useless use of constant omitted'.">
or something similar depending on your shell).  The default is '???'.
If you're using B::DeparseTree on a module or other file that's require'd,
you shouldn't use a value that evaluates to false, since the customary
true constant at the end of a module will be in void context when the
file is compiled as a main program.

=back

=item B<-x>I<LEVEL>

Expand conventional syntax constructions into equivalent ones that expose
their internal operation.  I<LEVEL> should be a digit, with higher values
meaning more expansion.  As with B<-q>, this actually involves turning off
special cases in B::DeparseTree's normal operations.

If I<LEVEL> is at least 3, C<for> loops will be translated into equivalent
while loops with continue blocks; for instance

    for ($i = 0; $i < 10; ++$i) {
        print $i;
    }

turns into

    $i = 0;
    while ($i < 10) {
        print $i;
    } continue {
        ++$i
    }

Note that in a few cases this translation can't be perfectly carried back
into the source code -- if the loop's initializer declares a my variable,
for instance, it won't have the correct scope outside of the loop.

If I<LEVEL> is at least 5, C<use> declarations will be translated into
C<BEGIN> blocks containing calls to C<require> and C<import>; for
instance,

    use strict 'refs';

turns into

    sub BEGIN {
        require strict;
        do {
            'strict'->import('refs')
        };
    }

If I<LEVEL> is at least 7, C<if> statements will be translated into
equivalent expressions using C<&&>, C<?:> and C<do {}>; for instance

    print 'hi' if $nice;
    if ($nice) {
        print 'hi';
    }
    if ($nice) {
        print 'hi';
    } else {
        print 'bye';
    }

turns into

    $nice and print 'hi';
    $nice and do { print 'hi' };
    $nice ? do { print 'hi' } : do { print 'bye' };

Long sequences of elsifs will turn into nested ternary operators, which
B::DeparseTree doesn't know how to indent nicely.

=back

=head1 USING B::DeparseTree AS A MODULE

=head2 Synopsis

    use B::DeparseTree;
    use B::DeparseTree::Printer qw(format_info);


    $deparse = B::DeparseTree->new("-sC");
    $body = $deparse->coderef2text(\&func);
    eval "sub func $body"; # the inverse operation

    # $deparse->{optree} is a hash indexed by opcode address
    while (my($key, $value) = each %{$deparse->{optree}}) {
	printf "Opcode 0x%x\n", $key;
	print format_info($value);
    }


=head2 Description

B::DeparseTree can also be used on a sub-by-sub basis from other perl
programs.

=head2 new

    $deparse = B::DeparseTree->new(OPTIONS)

Create an object to store the state of a deparsing operation and any
options.  The options are the same as those that can be given on the
command line (see L</OPTIONS>); options that are separated by commas
after B<-MO=DeparseTree> should be given as separate strings.

=head2 ambient_pragmas

    $deparse->ambient_pragmas(strict => 'all', '$[' => $[);

The compilation of a subroutine can be affected by a few compiler
directives, B<pragmas>.  These are:

=over 4

=item *

use strict;

=item *

use warnings;

=item *

Assigning to the special variable $[

=item *

use integer;

=item *

use bytes;

=item *

use utf8;

=item *

use re;

=back

Ordinarily, if you use B::DeparseTree on a subroutine which has
been compiled in the presence of one or more of these pragmas,
the output will include statements to turn on the appropriate
directives.  So if you then compile the code returned by coderef2text,
it will behave the same way as the subroutine which you deparsed.

However, you may know that you intend to use the results in a
particular context, where some pragmas are already in scope.  In
this case, you use the B<ambient_pragmas> method to describe the
assumptions you wish to make.

Not all of the options currently have any useful effect.  See
L</BUGS> for more details.

The parameters it accepts are:

=over 4

=item strict

Takes a string, possibly containing several values separated
by whitespace.  The special values "all" and "none" mean what you'd
expect.

    $deparse->ambient_pragmas(strict => 'subs refs');

=item $[

Takes a number, the value of the array base $[.
Cannot be non-zero on Perl 5.15.3 or later.

=item bytes

=item utf8

=item integer

If the value is true, then the appropriate pragma is assumed to
be in the ambient scope, otherwise not.

=item re

Takes a string, possibly containing a whitespace-separated list of
values.  The values "all" and "none" are special.  It's also permissible
to pass an array reference here.

    $deparser->ambient_pragmas(re => 'eval');


=item warnings

Takes a string, possibly containing a whitespace-separated list of
values.  The values "all" and "none" are special, again.  It's also
permissible to pass an array reference here.

    $deparser->ambient_pragmas(warnings => [qw[void io]]);

If one of the values is the string "FATAL", then all the warnings
in that list will be considered fatal, just as with the B<warnings>
pragma itself.  Should you need to specify that some warnings are
fatal, and others are merely enabled, you can pass the B<warnings>
parameter twice:

    $deparser->ambient_pragmas(
	warnings => 'all',
	warnings => [FATAL => qw/void io/],
    );

See L<warnings> for more information about lexical warnings.

=item hint_bits

=item warning_bits

These two parameters are used to specify the ambient pragmas in
the format used by the special variables $^H and ${^WARNING_BITS}.

They exist principally so that you can write code like:

    { my ($hint_bits, $warning_bits);
    BEGIN {($hint_bits, $warning_bits) = ($^H, ${^WARNING_BITS})}
    $deparser->ambient_pragmas (
	hint_bits    => $hint_bits,
	warning_bits => $warning_bits,
	'$['         => 0 + $[
    ); }

which specifies that the ambient pragmas are exactly those which
are in scope at the point of calling.

=item %^H

This parameter is used to specify the ambient pragmas which are
stored in the special hash %^H.

=back

=head2 coderef2text

    $body = $deparse->coderef2text(\&func)
    $body = $deparse->coderef2text(sub ($$) { ... })

Return source code for the body of a subroutine (a block, optionally
preceded by a prototype in parens), given a reference to the
sub.  Because a subroutine can have no names, or more than one name,
this method doesn't return a complete subroutine definition -- if you
want to eval the result, you should prepend "sub subname ", or "sub "
for an anonymous function constructor.  Unless the sub was defined in
the main:: package, the code will include a package declaration.

=head1 BUGS

=over 4

=item *

The only pragmas to be completely supported are: C<use warnings>,
C<use strict>, C<use bytes>, C<use integer>
and C<use feature>.  (C<$[>, which
behaves like a pragma, is also supported.)

Excepting those listed above, we're currently unable to guarantee that
B::DeparseTree will produce a pragma at the correct point in the program.
(Specifically, pragmas at the beginning of a block often appear right
before the start of the block instead.)
Since the effects of pragmas are often lexically scoped, this can mean
that the pragma holds sway over a different portion of the program
than in the input file.

=item *

In fact, the above is a specific instance of a more general problem:
we can't guarantee to produce BEGIN blocks or C<use> declarations in
exactly the right place.  So if you use a module which affects compilation
(such as by over-riding keywords, overloading constants or whatever)
then the output code might not work as intended.

This is the most serious outstanding problem, and will require some help
from the Perl core to fix.

=item *

Some constants don't print correctly either with or without B<-d>.
For instance, neither B::DeparseTree nor Data::Dumper know how to print
dual-valued scalars correctly, as in:

    use constant E2BIG => ($!=7); $y = E2BIG; print $y, 0+$y;

    use constant H => { "#" => 1 }; H->{"#"};

=item *

An input file that uses source filtering probably won't be deparsed into
runnable code, because it will still include the B<use> declaration
for the source filtering module, even though the code that is
produced is already ordinary Perl which shouldn't be filtered again.

=item *

Optimised away statements are rendered as
'???'.  This includes statements that
have a compile-time side-effect, such as the obscure

    my $x if 0;

which is not, consequently, deparsed correctly.

    foreach my $i (@_) { 0 }
  =>
    foreach my $i (@_) { '???' }

=item *

Lexical (my) variables declared in scopes external to a subroutine
appear in code2ref output text as package variables.  This is a tricky
problem, as perl has no native facility for referring to a lexical variable
defined within a different scope, although L<PadWalker> is a good start.

=item *

There are probably many more bugs on non-ASCII platforms (EBCDIC).

=item *

Lexical C<my> subroutines are not deparsed properly at the moment.  They are
emitted as pure declarations, without their body; and the declaration may
appear in the wrong place (before any lexicals the body closes over, or
before the C<use feature> declaration that permits use of this feature).

We expect to resolve this before the lexical-subroutine feature is no longer
considered experimental.

=item *

Lexical C<state> subroutines are not deparsed at all at the moment.

We expect to resolve this before the lexical-subroutine feature is no longer
considered experimental.

=back

=head1 AUTHOR

Stephen McCamant <smcc@CSUA.Berkeley.EDU>, based on an earlier version
by Malcolm Beattie <mbeattie@sable.ox.ac.uk>, with contributions from
Gisle Aas, James Duncan, Albert Dvornik, Robin Houston, Dave Mitchell,
Hugo van der Sanden, Gurusamy Sarathy, Nick Ing-Simmons, and Rafael
Garcia-Suarez.

Rocky Bernstein reworked this to save tree information.

=cut

1;
