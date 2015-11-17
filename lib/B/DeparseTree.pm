package B::DeparseTree;

use rlib '.';
use strict;
use vars qw(@ISA $VERSION);

$VERSION = '2.0.2';

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

1;

__END__
