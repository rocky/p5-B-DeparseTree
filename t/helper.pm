use File::Basename qw(dirname basename); use File::Spec;
use constant data_dir => File::Spec->catfile(dirname(__FILE__), 'testdata');

use strict; use warnings;
use B::DeparseTree;
use vars qw($deparse $deparse_orig %SEEN %SEEN_STRENGTH);
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
