package B::DeparseTree::OP;

our($VERSION, @EXPORT, @ISA);
$VERSION = '3.1.1';
@ISA = qw(Exporter);

# This hash maps a pp_ opname, like pp_accept into a sequence
# of functions that is invoked to implement the actions
# that need to be taken to handle that op.
#
# The way the paremeters are passed for functions is determined
# by looking at the name. For example "maybe_my()" wraps
# the next function in the array, while "listop" just passes
# args adding the key name as the last argument.

# FIXME: move in from Common.pm
# @EXPORT = qw(%PP_MAPFNS);
# my %PP_MAPFNS = (
#     );

1;
