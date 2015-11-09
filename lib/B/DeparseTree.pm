package B::DeparseTree;

use rlib '.';
use strict;
use vars qw(@ISA $VERSION);

$VERSION = '2.0.0';

my $module;
if ($] > 5.020 and $] < 5.22) {
    $module  = "P520";
} elsif ($] > 5.022) {
    $module  = "P522";
} else {
    die "Can only handle Perl 5.20 and 5.22";
}

require "B/DeparseTree/${module}.pm";
@ISA = ("B::DeparseTree::$module");

1;

__END__
