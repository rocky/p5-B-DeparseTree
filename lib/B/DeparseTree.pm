package B::DeparseTree;

use warnings; use strict;

our $VERSION = '3.1.0';

use rlib '.';
use vars qw(@ISA);

use base 'Exporter';

use Config;
my $is_cperl = $Config::Config{usecperl};

my $module;
if ($] >= 5.016 and $] < 5.018) {
    # 5.16 and 5.18 are the same for now
    $module = "P518";
} elsif ($] >= 5.018 and $] < 5.020) {
    $module = "P518";
} elsif ($] >= 5.020 and $] < 5.022) {
    $module = "P520";
} elsif ($] >= 5.022 and $] < 5.024) {
    $module = "P522";
} elsif ($] >= 5.024 and $] < 5.026) {
    $module = "P524";
} elsif ($] >= 5.026) {
    $module = "P526";
} else {
    die "Can only handle Perl 5.16..5.26";
}

$module .= 'c' if $is_cperl;

require "B/DeparseTree/${module}.pm";

# FIXME:
*compile = \&B::DeparseTree::Common::compile;
*new = \&B::DeparseTree::Common::new;
*init = \&B::DeparseTree::Common::init;
*coderef2info = \&B::DeparseTree::Common::coderef2info;
*coderef2text = \&B::DeparseTree::Common::coderef2text;
*deparse_sub = \&B::DeparseTree::Common::deparse_sub;
*pessimise = \&B::DeparseTree::Common::pessimise;
*_pessimise_walk = \&B::DeparseTree::Common::_pessimise_walk;
*_pessimise_walk_exe = \&B::DeparseTree::Common::_pessimise_walk_exe;
*lineseq = \&B::DeparseTree::Common::lineseq;
*walk_lineseq = \&B::DeparseTree::Common::walk_lineseq;
*deparse = \&B::DeparseTree::Common::deparse;

if (!$is_cperl) {
    @ISA = ("Exporter", "B::DeparseTree::$module");
}
our @EXPORT = qw(is_cperl);

1;

__END__
