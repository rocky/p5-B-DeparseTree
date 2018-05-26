# Copyright (c) 2018 Rocky Bernstein

# A table-driven mapping from Perl PP ops e.g pp_accept, pp_aeach,
# ... to a compact list of function names with arguments that
# implements them.

package B::DeparseTree::PP_OPtable;

use warnings; use strict;
our($VERSION, @EXPORT, @ISA);
$VERSION = '3.1.1';
@ISA = qw(Exporter);

use vars qw(%PP_MAPFNS);


# In the HASH below, the key is the operation name with the leading pp_ stripped.
# so "die" refers to function "pp_die". The value can be several things.
#
# If the table value is a string, then that function is called with a standard
# set of parameters. For example, consider:
#   'die' => 'listop'.
#
# The above indicates that for the pp_die operation we should call listop
# with standard parameters $self, $op, and the key value, e.g
# "die". This replaces B::Deparse equivalent:
#    sub pp_die { $self->listop($op, $cx, "die"); }
#
# If the table value is not a string, it will be an array reference, and here
# the entries may be subject to interpretation based on the function name.
#
# For the most part, when there are two entries, it is similar to the string case,
# but instead of passing the key name as a parameter, the string second parameter
# is used. For example:
#    'ghostent'   => ['baseop', "gethostent"],
# replaces the B::Deparse equivalent:
#    sub pp_ghostent { $self->listop($op, $cx, "gethostent"); }


%PP_MAPFNS = (
     # 'avalues'    => ['unop', 'value'],
     # 'values'     => 'unop', # FIXME
     # 'sselect'    => 'listop',  FIXME: is used in PPfns
     # 'sockpair'   => 'listop', ""

    'accept'     => 'listop',
    'aeach'      => ['unop', 'each'],
    'akeys'      => ['unop', 'keys'],
    'alarm'      => 'unop',

    'bind'       => 'listop',
    'binmode'    => 'listop',
    'bless'      => 'listop',
    'break'      => 'unop',

    'caller'     => 'unop',
    'chr'        => ['maybe_targmy', 'unop'],
    'chroot'     => ['maybe_targmy', 'unop'],
    'close'      => 'unop',
    'closedir'   => 'unop',
    'connect'    => 'listop',
    'continue'   => 'unop',

    'db_open'    => 'listop',
    'dbmclose'   => 'unop',
    'dbmopen'    => 'listop',
    # 'dbstate'    => 'nextstate',
    'defined'    => 'unop',
    'die'        => 'listop',
    'dump'       => ['loopex', "CORE::dump"],

    'each'       => 'unop',
    'egrent'     => ['baseop', 'endgrent'],
    'ehostent'   => ['baseop', "endhostent"],
    'enetent'    => ['baseop', "endnetent"],
    'eof'        => 'unop',
    'eprotoent'  => ['baseop', "endprotoent"],
    'epwent'     => ['baseop', "endpwent"],
    'eservent'   => ['baseop', "endservent"],
    'exit'       => 'unop',

    'fcntl'      => 'listop',
    'fileno'     => 'unop',
    'fork'       => 'baseop',
    'formline'   => 'listop', # see also deparse_format
    'ftatime'    => ['filetest', "-A"],
    'ftbinary'   => ['filetest', "-B"],
    'ftblk'      => ['filetest', "-b"],
    'ftchr'      => ['filetest', "-c"],
    'ftctime'    => ['filetest', "-C"],
    'ftdir'      => ['filetest', "-d"],
    'fteexec'    => ['filetest', "-x"],
    'fteowned'   => ['filetest', "-O"],
    'fteread'    => ['filetest', "-r"],
    'ftewrite'   => ['filetest', "-w"],
    'ftfile'     => ['filetest', "-f"],
    'ftis'       => ['filetest', "-e"],
    'ftlink'     => ['filetest', "-l"],
    'ftmtime'    => ['filetest', "-M"],
    'ftpipe'     => ['filetest', "-p"],
    'ftrexec'    => ['filetest', "-X"],
    'ftrowned'   => ['filetest', "-o"],
    'ftrread'    => ['filetest', '-R'],
    'ftrwrite'   => ['filetest', "-W"],
    'ftsgid'     => ['filetest', "-g"],
    'ftsize'     => ['filetest', "-s"],
    'ftsock'     => ['filetest', "-S"],
    'ftsuid'     => ['filetest', "-u"],
    'ftsvtx'     => ['filetest', "-k"],
    'fttext'     => ['filetest', "-T"],
    'fttty'      => ['filetest', "-t"],
    'ftzero'     => ['filetest', "-z"],

    'getc'       => 'unop',
    'getlogin'   => 'baseop',
    'getpeername' => 'unop',
    'getsockname' => 'unop',
    'ggrent'     => ['baseop', "getgrent"],
    'ggrgid'     => ['unop',   "getgrgid"],
    'ggrnam'     => ['unop',   "getgrnam"],
    'ghbyaddr'   => ['listop', 'gethostbyaddr'],
    'ghbyname'   => ['unop',   "gethostbyname"],
    'ghostent'   => ['baseop', "gethostent"],
    'gmtime'     => 'unop',
    'gnbyaddr'   => ['listop', "getnetbyaddr"],
    'gnbyname'   => ['unop',   "getnetbyname"],
    'gnetent'    => ['baseop', "getnetent"],
    'goto'       => ['loopex', "goto"],
    'gpbyname'   => ['unop',   "getprotobyname"],
    'gpbynumber' => ['listop', 'getprotobynumber'],
    'gprotoent'  => ['baseop', "getprotoent"],
    'gpwent'     => ['baseop', "getpwent"],
    'gpwnam'     => ['unop',   "getpwnam"],
    'gpwuid'     => ['unop',   "getpwuid"],
    'grepstart'  => ['baseop', "grep"],
    'gsbyname'   => ['listop', 'getservbyname'],
    'gsbyport'   => ['listop', 'getservbyport'],
    'gservent'   => ['baseop', "getservent"],
    'gsockopt'   => ['listop', 'getsockopt'],

    'ioctl'      => 'listop',
    'keys'       => 'unop',

    'last'       => 'loopex',
    'length'     => ['maybe_targmy', 'unop'],
    'listen'     => 'listop',
    'localtime'  => 'unop',
    'lock'       => 'unop',
    'lstat'      => 'filetest',

    'msgctl'     => 'listop',
    'msgget'     => 'listop',
    'msgrcv'     => 'listop',
    'msgsnd'     => 'listop',
    'ord'         => ['maybe_targmy', 'unop'],

    'next'       => 'loopex',
    'open'       => 'listop',

    'pack'       => 'listop',
    'pipe_op'    => ['listop', 'pipe'],
    'pop'        => 'unop',
    'prototype'  => 'unop',

    'read'       => 'listop',
    'readdir'    => 'unop',
    'readlink'   => 'unop',
    'recv'       => 'listop',
    'redo'       => 'loopex',
    'ref'        => 'unop',
    'reset'      => 'unop',
    'return'     => 'listop',
    'reverse'    => 'listop',
    'rewinddir'  => 'unop',

    'say'        => 'indirop',
    'seek'       => 'listop',
    'seekdir'    => 'listop',
    'select'     => 'listop',
    'semctl'     => 'listop',
    'semget'     => 'listop',
    'semop'      => 'listop',
    'send'       => 'listop',
    'setstate'   => 'nextstate',
    'sgrent'     => ['baseop', "setgrent"],
    'shift'      => 'unop',
    'shmctl'     => 'listop',
    'shmget'     => 'listop',
    'shmread'    => 'listop',
    'shmwrite'   => 'listop',
    'shostent'   => ['unop',   "sethostent"],
    'shutdown'   => 'listop',
    'snetent'    => ['unop',   "setnetent"],
    'socket'     => 'listop',
    'sort'       => "indirop",
    'splice'     => 'listop',
    'sprotoent'  => ['unop',   "setprotoent"],
    'spwent'     => ['baseop', "setpwent"],
    'srand'      => 'unop',
    'srefgen'    => 'refgen',
    'sselect'    => ['listop', "select"],
    'sservent'   => ['unop',   "setservent"],
    'ssockopt'   => ['listop', "setsockopt"],
    'stat'       => 'filetest',
    'study'      => 'unop',
    'syscall'    => 'listop',
    'sysopen'    => 'listop',
    'sysread'    => 'listop',
    'sysseek'    => 'listop',
    'syswrite'   => 'listop',

    'tell'       => 'unop',
    'telldir'    => 'unop',
    'tie'        => 'listop',
    'tied'       => 'unop',
    'tms'        => ['baseop', 'times'],

    'umask'      => 'unop',
    'undef'      => 'unop',
    'unpack'     => 'listop',
    'untie'      => 'unop',

    'warn'       => 'listop',
    );

@EXPORT = qw(%PP_MAPFNS);

1;
