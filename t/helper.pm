use File::Basename qw(dirname basename); use File::Spec;
use constant data_dir => File::Spec->catfile(dirname(__FILE__), 'testdata');

use rlib '../lib';
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

# test a keyword that is a binary infix operator, like 'cmp'.
# $parens - "$a op $b" is deparsed as "($a op $b)"
# $strong - keyword is strong

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
