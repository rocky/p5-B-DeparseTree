# Adapted from Perl 5.26.2's lib/B/Deparse.t
1;
__DATA__
####
# A constant
1;
####
# Constants in a block
# CONTEXT no warnings;
{
    '?unrecoverable constant?';
    2;
}
####
# List of constants in void context
# SKIP ?1
# CONTEXT no warnings;
(1,2,3);
0;
>>>>
'?unrecoverable constant?', '?unrecoverable constant?', '?unrecoverable constant?';
0;
####
# Lexical and simple arithmetic
my $test;
++$test and $test /= 2;
>>>>
my $test;
$test /= 2 if ++$test;
####
# list x
-((1, 2) x 2);
####
# Assignment to list x
# SKIP ?1
((undef) x 3) = undef;
####
# lvalue sub
{
    my $test = sub : lvalue {
	my $x
    }
    ;
}
####
# method
{
    my $test = sub : method {
	my $x
    }
    ;
}
####
# SKIP ?1
# anonsub attrs at statement start
my $x = do { +sub : lvalue { my $y; } };
my $z = do { foo: +sub : method { my $a; } };
####
# SKIP ?1
# block with continue
{
    234;
}
continue {
    123;
}
####
# lexical and package scalars
my $x;
print $main::x;
####
# lexical and package arrays
# SKIP ?1
my @x;
print $main::x[1];
print \my @a;
####
# lexical and package hashes
my %x;
$x{warn()};
####
# readline and rcatline
# SKIP ?$] < 5.023
# <>
my $foo;
$_ .= <> . <ARGV> . <$foo>;
<$foo>;
<${foo}>;
<$ foo>;
>>>>
my $foo;
$_ .= readline(ARGV) . readline(ARGV) . readline($foo);
readline $foo;
glob $foo;
glob $foo;
####
# block
{ my $x }
####
# while 1
while (1) { my $k }
####
# constants as method names
'Foo'->bar('orz');
####
# constants as method names without ()
'Foo'->bar;
####
# SKIP ?1
# [perl #47359] "indirect" method call notation
our @bar;
foo{@bar}+1,->foo;
(foo{@bar}+1),foo();
foo{@bar}1 xor foo();
>>>>
our @bar;
(foo { @bar } 1)->foo;
(foo { @bar } 1), foo();
foo { @bar } 1 xor foo();
####
# indirops with blocks
# CONTEXT use 5.01;
print {*STDOUT} 'foo';
printf {*STDOUT} 'foo';
say {*STDOUT} 'foo';
####
# SKIP ?$] < 5.010 && "say not implemented on this Perl version"
# say with use 5.10.0
use 5.10.0;
say 'foo'
>>>>
no feature ':all';
use feature ':5.10';
say 'foo';
####
# SKIP ?$] < 5.010 && "say not implemented on this Perl version"
# say with use feature ':5.10';
use feature ':5.10';
say 'foo';
>>>>
use feature 'say', 'state', 'switch';
say 'foo';
####
# SKIP ?$] < 5.010 && "say not implemented on this Perl version"
# CONTEXT use feature ':5.10';
# say with use 5.10.0 in the context of use feature
use 5.10.0;
say 'foo'
>>>>
no feature ':all';
use feature ':5.10';
say 'foo'
####
# CONTEXT use 5.10.0;
# say with use feature ':5.10' in the context of use 5.10.0
use feature ':5.10';
say 'foo';
>>>>
say 'foo'
####
# SKIP ?($] < 5.015 || $is_cperl) && "__SUB__ not implemented on this Perl version"
# CONTEXT use feature ':5.15';
# __SUB__
__SUB__;
####
# SKIP ?($] < 5.015 || $is_cperl) && "__SUB__ not implemented on this Perl version"
# CONTEXT use 5.15.0;
# __SUB__ in the context of use 5.15.0
__SUB__;
####
# SKIP ?$] < 5.015 && "__SUB__ not implemented on this Perl version"
# __SUB__ with use 5.15.0
use 5.15.0;
__SUB__;
>>>>
no feature ':all';
use feature ':5.16';
__SUB__;
####
# SKIP ?($] < 5.015 || $is_cperl) && "__SUB__ not implemented on this Perl version"
# __SUB__ with use feature ':5.15';
use feature ':5.15';
__SUB__;
>>>>
use feature 'current_sub', 'evalbytes', 'fc', 'say', 'state', 'switch', 'unicode_strings', 'unicode_eval';
__SUB__;
####
# SKIP ?($] < 5.015 || $is_cperl) && "__SUB__ not implemented on this Perl version"
# CONTEXT use feature ':5.15';
# __SUB__ with use 5.15.0 in the context of use feature
use 5.15.0;
__SUB__;
>>>>
no feature ':all';
use feature ':5.16';
__SUB__;
####
# SKIP ?($] < 5.015 || $is_cperl) && "__SUB__ not implemented on this Perl version"
# CONTEXT use 5.15.0;
# __SUB__ with use feature ':5.15' in the context of use 5.15.0
use feature ':5.15';
__SUB__;
>>>>
__SUB__;
####
# SKIP (?$] < 5.010 || $is_cperl) && "state vars not implemented on this Perl version"
# CONTEXT use feature ':5.10';
# state vars
state $x = 42;
####
# tests with not, not optimized
my $c;
x() unless $a;
x() if not $a and $b;
x() if $a and not $b;
x() unless not $a and $b;
x() unless $a and not $b;
x() if not $a or $b;
x() if $a or not $b;
x() unless not $a or $b;
x() unless $a or not $b;
x() if $a and not $b and $c;
x() if not $a and $b and not $c;
x() unless $a and not $b and $c;
x() unless not $a and $b and not $c;
x() if $a or not $b or $c;
x() if not $a or $b or not $c;
x() unless $a or not $b or $c;
x() unless not $a or $b or not $c;
####
# tests with not, optimized
my $c;
x() if not $a;
x() unless not $a;
x() if not $a and not $b;
x() unless not $a and not $b;
x() if not $a or not $b;
x() unless not $a or not $b;
x() if not $a and not $b and $c;
x() unless not $a and not $b and $c;
x() if not $a or not $b or $c;
x() unless not $a or not $b or $c;
x() if not $a and not $b and not $c;
x() unless not $a and not $b and not $c;
x() if not $a or not $b or not $c;
x() unless not $a or not $b or not $c;
x() unless not $a or not $b or not $c;
>>>>
my $c;
x() unless $a;
x() if $a;
x() unless $a or $b;
x() if $a or $b;
x() unless $a and $b;
x() if $a and $b;
x() if not $a || $b and $c;
x() unless not $a || $b and $c;
x() if not $a && $b or $c;
x() unless not $a && $b or $c;
x() unless $a or $b or $c;
x() if $a or $b or $c;
x() unless $a and $b and $c;
x() if $a and $b and $c;
x() unless not $a && $b && $c;
####
# no attribute list
my $pi = 4;
####
# SKIP ?$] > 5.013006 && ":= is now a syntax error"
# := treated as an empty attribute list
no warnings;
my $pi := 4;
>>>>
no warnings;
my $pi = 4;
####
# : = empty attribute list
my $pi : = 4;
>>>>
my $pi = 4;
####
# in place sort
our @a;
my @b;
@main::a = sort @main::a;
@b = sort @b;
();
####
# in place reverse
# SKIP ?1
our @a;
my @b;
@main::a = reverse @a;
@b = reverse @b;
();
####
# #71870 Use of uninitialized value in bitwise and B::Deparse
# SKIP ?1
my($r, $s, @a);
@a = split(/foo/, $s, 0);
$r = qr/foo/;
@a = split(/$r/, $s, 0);
();
####
# SKIP ?1
# package declaration before label
{
    package Foo;
    label: print 123;
}
####
# shift optimisation
shift;
>>>>
shift();
####
# shift optimisation
shift @_;
####
# shift optimisation
pop;
>>>>
pop();
####
# shift optimisation
pop @_;
####
#[perl #20444]
"foo" =~ (1 ? /foo/ : /bar/);
"foo" =~ (1 ? y/foo// : /bar/);
"foo" =~ (1 ? y/foo//r : /bar/);
"foo" =~ (1 ? s/foo// : /bar/);
>>>>
'foo' =~ ($_ =~ /foo/);
'foo' =~ ($_ =~ tr/fo//);
'foo' =~ ($_ =~ tr/fo//r);
'foo' =~ ($_ =~ s/foo//)
# ####
# # The fix for [perl #20444] broke this.
# 'foo' =~ do { () };
####
# [perl #81424] match against aelemfast_lex
my @s;
print /$s[1]/;
####
# all the flags (tr///)
# SKIP
tr/X/Y/c;
tr/X//d;
tr/X//s;
tr/X//r;
####
# [perl #91008]
# SKIP ?($] >= 5.023 || $] < 5.019) && "autoderef deleted in this Perl version"
# CONTEXT no warnings 'experimental::autoderef';
each $@;
keys $~;
values $!;
####
# readpipe with complex expression
readpipe $a + $b;
####
# aelemfast
$b::a[0] = 1;
####
# aelemfast for a lexical
my @a;
$a[0] = 1;
####
# $#- $#+ $#{%} etc.
# SKIP ?$] < 5.023
my @x;
@x = ($#{`}, $#{~}, $#{!}, $#{@}, $#{$}, $#{%}, $#{^}, $#{&}, $#{*});
@x = ($#{(}, $#{)}, $#{[}, $#{{}, $#{]}, $#{}}, $#{'}, $#{"}, $#{,});
@x = ($#{<}, $#{.}, $#{>}, $#{/}, $#{?}, $#{=}, $#+, $#{\}, $#{|}, $#-);
@x = ($#{;}, $#{:}, $#{1}), $#_
# ####
# # [perl #86060] $( $| $) in regexps need braces
# /${(}/;
# /${|}/;
# /${)}/;
# /${(}${|}${)}/;
# /@{+}@{-}/;
####
# ()[...]
my(@a) = ()[()];
####
# sort(foo(bar))
# sort(foo(bar)) is interpreted as sort &foo(bar)
# sort foo(bar) is interpreted as sort foo bar
# parentheses are not optional in this case
print sort(foo('bar'));
>>>>
print sort(foo('bar'));
####
# substr assignment
# SKIP $?
substr(my $a, 0, 0) = (foo(), bar());
$a++;
# ####
# # This following line works around an unfixed bug that we are not trying to
# # test for here:
# # CONTEXT BEGIN { $^H{a} = "b"; delete $^H{a} } # make %^H localised
# # hint hash
# BEGIN { $^H{'foo'} = undef; }
# {
#  BEGIN { $^H{'bar'} = undef; }
#  {
#   BEGIN { $^H{'baz'} = undef; }
#   {
#    print $_;
#   }
#   print $_;
#  }
#  print $_;
# }
# BEGIN { $^H{q[']} = '('; }
# print $_;
# ####
# # This following line works around an unfixed bug that we are not trying to
# # test for here:
# # CONTEXT BEGIN { $^H{a} = "b"; delete $^H{a} } # make %^H localised
# # hint hash changes that serialise the same way with sort %hh
# BEGIN { $^H{'a'} = 'b'; }
# {
#  BEGIN { $^H{'b'} = 'a'; delete $^H{'a'}; }
#  print $_;
# }
# print $_;
####
# Precedence conundrums with argument-less function calls
() = (eof) + 1;
() = (return) + 1;
() = (return, 1);
() = warn;
() = warn() + 1;
() = setpgrp() + 1;
####
# [perl #63558] open local(*FH)
open local *FH;
pipe local *FH, local *FH;
####
# require <binop>
require 'a' . $1;
####
# 'my' works with padrange op
my($z, @z);
my $m1;
$m1 = 1;
$z = $m1;
my $m2 = 2;
my($m3, $m4);
($m3, $m4) = (1, 2);
@z = ($m3, $m4);
my($m5, $m6) = (1, 2);
my($m7, undef, $m8) = (1, 2, 3);
@z = ($m7, undef, $m8);
($m7, undef, $m8) = (1, 2, 3);
####
# 'state' works with padrange op
# CONTEXT no strict; use feature 'state';
state($z, @z);
state $s1;
$s1 = 1;
$z = $s1;
state $s2 = 2;
state($s3, $s4);
($s3, $s4) = (1, 2);
@z = ($s3, $s4);
# assignment of state lists isn't implemented yet
#state($s5, $s6) = (1, 2);
#state($s7, undef, $s8) = (1, 2, 3);
#@z = ($s7, undef, $s8);
($s7, undef, $s8) = (1, 2, 3);
####
# slices with padrange
# SKIP ?$] < 5.023
my($a, $b);
my(@x, %y);
@x = @x[$a, $b];
@x = @y{$a, $b};
####
# binops with padrange
my($a, $b, $c);
$c = $a cmp $b;
$c = $a + $b;
$a += $b;
$c = $a - $b;
$a -= $b;
$c = my $a1 cmp $b;
$c = my $a2 + $b;
$a += my $b1;
$c = my $a3 - $b;
$a -= my $b2;
####
# @_ with padrange
my($a, $b, $c) = @_;
####
# Elements of %# should not be confused with $#{ array }
() = ${#}{'foo'};
####
# coderef2text and prototyped sub calls [perl #123435]
is 'foo', 'oo';
####
# exists $a[0]
our @a;
exists $a[0];
####
# my @a; exists $a[0]
my @a;
exists $a[0];
####
# delete $a[0]
our @a;
delete $a[0];
####
# my @a; delete $a[0]
my @a;
delete $a[0];
####
# all the flags (tr///)
tr/X/Y/c;
tr/X//d;
tr/X//s;
tr/X//r;
####
use feature 'unicode_strings';
s/X//d;
####
# 5.22 bitops
# SKIP ?1 && "start fixing here"
# SKIP ?$] < 5.022
# CONTEXT use feature "bitwise"; no warnings "experimental::bitwise";
$_ = $_ | $_;
$_ = $_ & $_;
$_ = $_ ^ $_;
$_ = ~$_;
$_ = $_ |. $_;
$_ = $_ &. $_;
$_ = $_ ^. $_;
$_ = ~.$_;
$_ |= $_;
$_ &= $_;
$_ ^= $_;
$_ |.= $_;
$_ &.= $_;
$_ ^.= $_;
####
# [perl #77096] functions that do not follow the llafr
() = (return 1) + time;
() = (return ($1 + $2) * $3) + time;
() = (return ($a xor $b)) + time;
() = (do 'file') + time;
() = (do ($1 + $2) * $3) + time;
() = (do ($1 xor $2)) + time;
() = (require 'foo') + 3;
() = (-R $_) + 3;
() = (-W $_) + 3;
() = (-X $_) + 3;
() = (-r $_) + 3;
() = (-w $_) + 3;
() = (-x $_) + 3;
