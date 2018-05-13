use File::Basename qw(dirname basename); use File::Spec;
use constant data_dir => File::Spec->catfile(dirname(__FILE__), 'testdata');

use strict; use warnings;
use B::DeparseTree;
use vars qw($deparse $deparse_orig %SEEN %SEEN_STRENGTH %infix_map);
$deparse = new B::DeparseTree;
use B::Deparse;
$deparse_orig = new B::Deparse;

BEGIN {
    require Config;
    if (($Config::Config{extensions} !~ /\bB\b/) ){
        print "1..0 # Skip -- Perl configured without B module\n";
        exit 0;
    }
    use Test::More;
}

# Deparse can't distinguish 'and' from '&&' etc
%infix_map = qw(and && or ||);

sub testit {
    my ($keyword, $expr, $expected_expr) = @_;

    $expected_expr //= $expr;
    $SEEN{$keyword} = 1;


    # lex=0:   () = foo($a,$b,$c)
    # lex=1:   my ($a,$b); () = foo($a,$b,$c)
    # lex=2:   () = foo(my $a,$b,$c)
    #for my $lex (0, 1, 2) {
    for my $lex (0, 1) {
	if ($lex) {
	    next if $keyword =~ /local|our|state|my/;
	}
	my $vars = $lex == 1 ? 'my($a, $b, $c, $d, $e);' . "\n    " : "";

	if ($lex == 2) {
	    my $repl = 'my $a';
	    if ($expr =~ /\bmap\(\$a|CORE::(chomp|chop|lstat|stat)\b/) {
		# for some reason only these do:
		#  'foo my $a, $b,' => foo my($a), $b, ...
		#  the rest don't parenthesize the my var.
		$repl = 'my($a)';
	    }
	    s/\$a/$repl/ for $expr, $expected_expr;
	}

	my $desc = "$keyword: lex=$lex $expr => $expected_expr";


	my $code_ref;
	{
	    package test;
	    use subs ();
	    import subs $keyword;
	    $code_ref = eval "no strict 'vars'; sub { ${vars}() = $expr }"
			    or die "$@ in $expr";
	}

	my $got_text = $deparse->coderef2text($code_ref);
	my $got_text_orig = $deparse_orig->coderef2text($code_ref);

	if ($got_text ne $got_text_orig) {

	    unless ($got_text =~ /^{
    package test;
    BEGIN \{\$\{\^WARNING_BITS} = "[^"]*"}
    use strict 'refs', 'subs';
    use feature [^\n]+
    \Q$vars\E\(\) = (.*)
}/s) {
		::fail($desc);
		::diag("couldn't extract line from boilerplate\n");
		::diag($got_text);
		return;
	    }

	    my $got_expr = $1;
	    is $got_expr, $expected_expr, $desc;
	}
    }
}

sub open_data($)
{
    my ($default_fn) = @_;
    my $short_name = $ARGV[0] || $default_fn;
    my $test_data = File::Spec->catfile(data_dir, $short_name);
    open(my $data_fh, "<", $test_data) || die "Can't open $test_data: $!";

    # Skip to __DATA__
    while (<$data_fh> !~ /__DATA__/) {
	;
    }
    return $data_fh;
}
1;
