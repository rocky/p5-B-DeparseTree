package B::DeparseTree;

use rlib '.';
use strict;
use vars qw(@ISA $VERSION);

$VERSION = '2.0.0';

my $module;
if ($] > 5.020 and $] < 5.022) {
    use B::DeparseTree::P520;
    $module = "P520";
    *compile = \&B::DeparseTree::P520::compile;
} elsif ($] >= 5.022) {
    use B::DeparseTree::P522;
    $module = "P522";
    *compile = \&B::DeparseTree::P522::compile;
} else {
    die "Can only handle Perl 5.20 and 5.22";
}

@ISA = ("B::DeparseTree::$module");

1;

__END__
