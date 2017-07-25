use warnings;
use strict;
use Test::More;
use English;
use File::Basename;

if ($] < 5.022 || $] > 5.0229) {
    plan skip_all => 'Customized to Perl 5.22 interpreter';
}

my $tests = 26; # not counting those in the __DATA__ section

use B::Deparse;
my $deparse = B::Deparse->new();
isa_ok($deparse, 'B::Deparse', 'instantiate a B::Deparse object');
my %deparse;

my $data_file = File::Basename::dirname(__FILE__) . '/testdata/P522.pm';

local $INPUT_RECORD_SEPARATOR = "\n";
open(my $fh, '<', $data_file) or die $!;
while (<$fh>) {
    chomp;
    last if $_ eq '__DATA__'
}

local $INPUT_RECORD_SEPARATOR = "\n####\n";
while (<$fh>) {
    chomp;
    $tests ++;
    # This code is pinched from the t/lib/common.pl for TODO.
    # It's not clear how to avoid duplication
    my %meta = (context => '');
    foreach my $what (qw(skip todo context options)) {
	s/^#\s*\U$what\E\s*(.*)\n//m and $meta{$what} = $1;
	# If the SKIP reason starts ? then it's taken as a code snippet to
	# evaluate. This provides the flexibility to have conditional SKIPs
	if ($meta{$what} && $meta{$what} =~ s/^\?//) {
	    my $temp = eval $meta{$what};
	    if ($@) {
		die "# In \U$what\E code reason:\n# $meta{$what}\n$@";
	    }
	    $meta{$what} = $temp;
	}
    }

    s/^\s*#\s*(.*)$//mg;
    my $desc = $1;
    die "Missing name in test $_" unless defined $desc;

    if ($meta{skip}) {
	# Like this to avoid needing a label SKIP:
	Test::More->builder->skip($meta{skip});
	next;
    }

    my ($input, $expected);
    if (/(.*)\n>>>>\n(.*)/s) {
	($input, $expected) = ($1, $2);
    }
    else {
	($input, $expected) = ($_, $_);
    }

    # parse options if necessary
    my $deparse = $meta{options}
	? $deparse{$meta{options}} ||=
	    new B::Deparse split /,/, $meta{options}
	: $deparse;

    my $coderef = eval "$meta{context};\n" . <<'EOC' . "sub {$input\n}";
# Tell B::Deparse about our ambient pragmas
my ($hint_bits, $warning_bits, $hinthash);
BEGIN {
    ($hint_bits, $warning_bits, $hinthash) = ($^H, ${^WARNING_BITS}, \%^H);
}
$deparse->ambient_pragmas (
    hint_bits    => $hint_bits,
    warning_bits => $warning_bits,
    '%^H'        => $hinthash,
);
EOC

    local $::TODO = $meta{todo};
    if ($@) {
	is($@, "", "compilation of $desc");
    }
    else {
	my $deparsed = $deparse->coderef2text( $coderef );
	my $regex = $expected;
	$regex =~ s/(\S+)/\Q$1/g;
	$regex =~ s/\s+/\\s+/g;
	$regex = '^\{\s*' . $regex . '\s*\}$';

        like($deparsed, qr/$regex/, $desc);
    }
}

# Reset the ambient pragmas
{
    my ($b, $w, $h);
    BEGIN {
        ($b, $w, $h) = ($^H, ${^WARNING_BITS}, \%^H);
    }
    $deparse->ambient_pragmas (
        hint_bits    => $b,
        warning_bits => $w,
        '%^H'        => $h,
    );
}

use constant 'c', 'stuff';
is((eval "sub ".$deparse->coderef2text(\&c))->(), 'stuff',
   'the subroutine generated by use constant deparses');

my $a = 0;
is($deparse->coderef2text(sub{(-1) ** $a }), "{\n    (-1) ** \$a;\n}",
   'anon sub capturing an external lexical');

use constant cr => ['hello'];
my $string = "sub " . $deparse->coderef2text(\&cr);
my $val = (eval $string)->() or diag $string;
is(ref($val), 'ARRAY', 'constant array references deparse');
is($val->[0], 'hello', 'and return the correct value');

my $path = join " ", map { qq["-I$_"] } @INC;

$a = `$^X $path "-MO=Deparse" -anlwi.bak -e 1 2>&1`;
$a =~ s/-e syntax OK\n//g;
$a =~ s/.*possible typo.*\n//;	   # Remove warning line
$a =~ s/.*-i used with no filenames.*\n//;	# Remove warning line
$b = quotemeta <<'EOF';
BEGIN { $^I = ".bak"; }
BEGIN { $^W = 1; }
BEGIN { $/ = "\n"; $\ = "\n"; }
LINE: while (defined($_ = <ARGV>)) {
    chomp $_;
    our(@F) = split(' ', $_, 0);
    '???';
}
EOF
$b =~ s/our\\\(\\\@F\\\)/our[( ]\@F\\)?/; # accept both our @F and our(@F)
like($a, qr/$b/,
   'command line flags deparse as BEGIN blocks setting control variables');

$a = `$^X $path "-MO=Deparse" -e "use constant PI => 4" 2>&1`;
$a =~ s/-e syntax OK\n//g;
is($a, "use constant ('PI', 4);\n",
   "Proxy Constant Subroutines must not show up as (incorrect) prototypes");

#Re: perlbug #35857, patch #24505
#handle warnings::register-ed packages properly.
package B::Deparse::Wrapper;
use strict;
use warnings;
use warnings::register;
sub getcode {
   my $deparser = B::Deparse->new();
   return $deparser->coderef2text(shift);
}

package Moo;
use overload '0+' => sub { 42 };

package main;
use strict;
use warnings;
use constant GLIPP => 'glipp';
use constant PI => 4;
use constant OVERLOADED_NUMIFICATION => bless({}, 'Moo');
use Fcntl qw/O_TRUNC O_APPEND O_EXCL/;
BEGIN { delete $::Fcntl::{O_APPEND}; }
use POSIX qw/O_CREAT/;
sub test {
   my $val = shift;
   my $res = B::Deparse::Wrapper::getcode($val);
   like($res, qr/use warnings/,
	'[perl #35857] [PATCH] B::Deparse doesnt handle warnings register properly');
}
my ($q,$p);
my $x=sub { ++$q,++$p };
test($x);
eval <<EOFCODE and test($x);
   package bar;
   use strict;
   use warnings;
   use warnings::register;
   package main;
   1
EOFCODE

# Exotic sub declarations
$a = `$^X $path "-MO=Deparse" -e "sub ::::{}sub ::::::{}" 2>&1`;
$a =~ s/-e syntax OK\n//g;
is($a, <<'EOCODG', "sub :::: and sub ::::::");
sub :::: {
    
}
sub :::::: {
    
}
EOCODG

# [perl #117311]
$a = `$^X $path "-MO=Deparse,-l" -e "map{ eval(0) }()" 2>&1`;
$a =~ s/-e syntax OK\n//g;
is($a, <<'EOCODH', "[perl #117311] [PATCH] -l option ('#line ...') does not emit ^Ls in the output");
#line 1 "-e"
map {
#line 1 "-e"
eval 0;} ();
EOCODH

# [perl #33752]
{
  my $code = <<"EOCODE";
{
    our \$\x{1e1f}\x{14d}\x{14d};
}
EOCODE
  my $deparsed
   = $deparse->coderef2text(eval "sub { our \$\x{1e1f}\x{14d}\x{14d} }" );
  s/$ \n//x for $deparsed, $code;
  is $deparsed, $code, 'our $funny_Unicode_chars';
}

# [perl #62500]
$a =
  `$^X $path "-MO=Deparse" -e "BEGIN{*CORE::GLOBAL::require=sub{1}}" 2>&1`;
$a =~ s/-e syntax OK\n//g;
is($a, <<'EOCODF', "CORE::GLOBAL::require override causing panick");
sub BEGIN {
    *CORE::GLOBAL::require = sub {
        1;
    }
    ;
}
EOCODF

# [perl #91384]
$a =
  `$^X $path "-MO=Deparse" -e "BEGIN{*Acme::Acme:: = *Acme::}" 2>&1`;
like($a, qr/-e syntax OK/,
    "Deparse does not hang when traversing stash circularities");

# [perl #93990]
@] = ();
is($deparse->coderef2text(sub{ print "foo@{]}" }),
q<{
    print "foo@{]}";
}>, 'curly around to interpolate "@{]}"');
is($deparse->coderef2text(sub{ print "foo@{-}" }),
q<{
    print "foo@-";
}>, 'no need to curly around to interpolate "@-"');

# Strict hints in %^H are mercilessly suppressed
$a =
  `$^X $path "-MO=Deparse" -e "use strict; print;" 2>&1`;
unlike($a, qr/BEGIN/,
    "Deparse does not emit strict hh hints");

# ambient_pragmas should not mess with strict settings.
SKIP: {
    skip "requires 5.11", 1 unless $] >= 5.011;
    eval q`
	BEGIN {
	    # Clear out all hints
	    %^H = ();
	    $^H = 0;
	    new B::Deparse -> ambient_pragmas(strict => 'all');
	}
	use 5.011;  # should enable strict
	ok !eval '$do_noT_create_a_variable_with_this_name = 1',
	  'ambient_pragmas do not mess with compiling scope';
   `;
}

# multiple statements on format lines
$a = `$^X $path "-MO=Deparse" -e "format =" -e "\@" -e "x();z()" -e. 2>&1`;
$a =~ s/-e syntax OK\n//g;
is($a, <<'EOCODH', 'multiple statements on format lines');
format STDOUT =
@
x(); z()
.
EOCODH

# is runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path, '-T' ],
#            prog => "format =\n\@\n\$;\n.\n"),
#    <<'EOCODM', '$; on format line';
# format STDOUT =
# @
# $;
# .
# EOCODM

# is runperl(stderr => 1, switches => [ '-MO=-qq,Deparse,-l', $path ],
#            prog => "format =\n\@\n\$foo\n.\n"),
#    <<'EOCODM', 'formats with -l';
# format STDOUT =
# @
# $foo
# .
# EOCODM

# is runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#            prog => "{ my \$x; format =\n\@\n\$x\n.\n}"),
#    <<'EOCODN', 'formats nested inside blocks';
# {
#     my $x;
#     format STDOUT =
# @
# $x
# .
# }
# EOCODN

# CORE::format
$a = readpipe qq`$^X $path "-MO=Deparse" -e "use feature q|:all|;`
             .qq` my sub format; CORE::format =" -e. 2>&1`;
like($a, qr/CORE::format/, 'CORE::format when lex format sub is in scope');

# literal big chars under 'use utf8'
is($deparse->coderef2text(sub{ use utf8; /€/; }),
'{
    /\x{20ac}/;
}',
"qr/euro/");

# STDERR when deparsing sub calls
# For a short while the output included 'While deparsing'
$a = `$^X $path "-MO=Deparse" -e "foo()" 2>&1`;
$a =~ s/-e syntax OK\n//g;
is($a, <<'EOCODI', 'no extra output when deparsing foo()');
foo();
EOCODI

# # Sub calls compiled before importation
# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => 'BEGIN {
#                        require Test::More;
#                        Test::More::->import;
#                        is(*foo, *foo)
#                      }'),
#      qr/&is\(/,
#     'sub calls compiled before importation of prototype subs';

# # [perl #121050] Prototypes with whitespace
# is runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#            prog => <<'EOCODO'),
# sub _121050(\$ \$) { }
# _121050($a,$b);
# sub _121050empty( ) {}
# () = _121050empty() + 1;
# EOCODO
#    <<'EOCODP', '[perl #121050] prototypes with whitespace';
# sub _121050 (\$ \$) {
    
# }
# _121050 $a, $b;
# sub _121050empty ( ) {
    
# }
# () = _121050empty + 1;
# EOCODP

# CORE::no
$a = readpipe qq`$^X $path "-MO=Deparse" -Xe `
             .qq`"use feature q|:all|; my sub no; CORE::no less" 2>&1`;
like($a, qr/my sub no;\nCORE::no less;/,
    'CORE::no after my sub no');

# CORE::use
$a = readpipe qq`$^X $path "-MO=Deparse" -Xe `
             .qq`"use feature q|:all|; my sub use; CORE::use less" 2>&1`;
like($a, qr/my sub use;\nCORE::use less;/,
    'CORE::use after my sub use');

# CORE::__DATA__
$a = readpipe qq`$^X $path "-MO=Deparse" -Xe `
             .qq`"use feature q|:all|; my sub __DATA__; `
             .qq`CORE::__DATA__" 2>&1`;
like($a, qr/my sub __DATA__;\n.*\nCORE::__DATA__/s,
    'CORE::__DATA__ after my sub __DATA__');

# # sub declarations
# $a = readpipe qq`$^X $path "-MO=Deparse" -e "sub foo{}" 2>&1`;
# like($a, qr/sub foo\s*\{\s+\}/, 'sub declarations');
# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#            prog => 'sub f($); sub f($){}'),
#      qr/sub f\s*\(\$\)\s*\{\s*\}/,
#     'predeclared prototyped subs';
# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#            prog => 'use Scalar::Util q-weaken-;
#                     sub f($);
#                     BEGIN { weaken($_=\$::{f}) }'),
#      qr/sub f\s*\(\$\)\s*;/,
#     'prototyped stub with weak reference to the stash entry';
# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#            prog => 'sub f () { 42 }'),
#      qr/sub f\s*\(\)\s*\{\s*42;\s*\}/,
#     'constant perl sub declaration';

# BEGIN blocks
SKIP : {
    skip "BEGIN output is wrong on old perls", 1 if $] < 5.021006;
    my $prog = '
      BEGIN { pop }
      {
        BEGIN { pop }
        {
          no overloading;
          {
            BEGIN { pop }
            die
          }
        }
      }';
    $prog =~ s/\n//g;
    $a = readpipe qq`$^X $path "-MO=Deparse" -e "$prog" 2>&1`;
    $a =~ s/-e syntax OK\n//g;
    is($a, <<'EOCODJ', 'BEGIN blocks');
sub BEGIN {
    pop @ARGV;
}
{
    sub BEGIN {
        pop @ARGV;
    }
    {
        no overloading;
        {
            sub BEGIN {
                pop @ARGV;
            }
            die;
        }
    }
}
EOCODJ
}
# is runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ], prog => '
#       {
#         {
#           die;
#           BEGIN { pop }
#         }
#         BEGIN { pop }
#       }
#       BEGIN { pop }
#   '), <<'EOCODL', 'BEGIN blocks at the end of their enclosing blocks';
# {
#     {
#         die;
#         sub BEGIN {
#             pop @ARGV;
#         }
#     }
#     sub BEGIN {
#         pop @ARGV;
#     }
# }
# sub BEGIN {
#     pop @ARGV;
# }
# EOCODL

# # BEGIN blocks should not be called __ANON__
# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => 'sub BEGIN { } CHECK { delete $::{BEGIN} }'),
#      qr/sub BEGIN/, 'anonymised BEGIN';

# # [perl #115066]
# my $prog = 'use constant FOO => do { 1 }; no overloading; die';
# $a = readpipe qq`$^X $path "-MO=-qq,Deparse" -e "$prog" 2>&1`;
# is($a, <<'EOCODK', '[perl #115066] use statements accidentally nested');
# use constant ('FOO', do {
#     1
# });
# no overloading;
# die;
# EOCODK

# # BEGIN blocks inside predeclared subs
# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => '
#                  sub run_tests;
#                  run_tests();
#                  sub run_tests { BEGIN { } die }'),
#      qr/sub run_tests \{\s*sub BEGIN/,
#     'BEGIN block inside predeclared sub';

# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => 'package foo; use overload qr=>sub{}'),
#      qr/package foo;\s*use overload/,
#     'package, then use';

# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => 'use feature lexical_subs=>; my sub f;sub main::f{}'),
#      qr/^sub main::f \{/m,
#     'sub decl when lex sub is in scope';

# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => 'sub foo{foo()}'),
#      qr/^sub foo \{\s+foo\(\)/m,
#     'recursive sub';

# like runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => 'use feature lexical_subs=>state=>;
#                       state sub sb5; sub { sub sb5 { } }'),
#      qr/sub \{\s*\(\);\s*sub sb5 \{/m,
#     'state sub in anon sub but declared outside';

# is runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path ],
#              prog => 'BEGIN { $::{f}=\!0 }'),
#    "sub BEGIN {\n    \$main::{'f'} = \\1;\n}\n",
#    '&PL_sv_yes constant (used to croak)';

# is runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path, '-T' ],
#            prog => '$x =~ (1?/$a/:0)'),
#   '$x =~ ($_ =~ /$a/);'."\n",
#   '$foo =~ <branch-folded match> under taint mode';

# unlike runperl(stderr => 1, switches => [ '-MO=-qq,Deparse', $path, '-w' ],
#                prog => 'BEGIN { undef &foo }'),
#        qr'Use of uninitialized value',
#       'no warnings for undefined sub';

done_testing($tests);
