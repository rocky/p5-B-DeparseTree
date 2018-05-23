package B::DeparseTree::OP;

our($VERSION, @EXPORT, @ISA);
$VERSION = '3.1.1';
@ISA = qw(Exporter);
@EXPORT = qw(%PP_MAPFNS);

# This hash maps a pp_ opname, like pp_accept into a sequence
# of functions that is invoked to implement the actions
# that need to be taken to handle that op.
#
# The way the paremeters are passed for functions is determined
# by looking at the name. For example "maybe_my()" wraps
# the next function in the array, while "listop" just passes
# args adding the key name as the last argument.

my %PP_MAPFNS = (
    'accept'     => ('listop'),
    'bind'       => ('listop'),
    'binmode'    => ('listop'),
    'bless'      => ('listop'),
    'connect'    => ('listop'),
    'db_open'    => ('listop'),
    'die'        => ('listop'),
    'fcntl'      => ('listop'),
    'formline'   => ('listop'), # see also deparse_format

    'ghbyaddr'   => ('listop'),
    'gnbyaddr'   => ('listop'),
    'gpbynumber' => ('listop'),
    'gsbyname'   => ('listop'),
    'gsbyport'   => ('listop'),
    'gsockopt'   => ('listop'),
    'ioctl'      => ('listop'),
    'listen'     => ('listop'),
    'msgctl'     => ('listop'),
    'msgget'     => ('listop'),
    'msgrcv'     => ('listop'),
    'msgsnd'     => ('listop'),
    'open'       => ('listop'),
    'pack'       => ('listop'),
    'pipe_op'    => ('listop'),
    'read'       => ('listop'),
    'recv'       => ('listop'),
    'return'     => ('listop'),
    'reverse'    => ('listop'),
    'seek'       => ('listop'),
    'seekdir'    => ('listop'),
    'select'     => ('listop'),
    'semctl'     => ('listop'),
    'semget'     => ('listop'),
    'semop'      => ('listop'),
    'send'       => ('listop'),
    'shmctl'     => ('listop'),
    'shmget'     => ('listop'),
    'shmread'    => ('listop'),
    'shmwrite'   => ('listop'),
    'shutdown'   => ('listop'),
    'syscall'    => ('listop'),
    'sysopen'    => ('listop'),
    'sysread'    => ('listop'),
    'sysseek'    => ('listop'),
    'syswrite'   => ('listop'),
    'unpack'     => ('listop'),
    'warn'       => ('listop'),
    );

1;
