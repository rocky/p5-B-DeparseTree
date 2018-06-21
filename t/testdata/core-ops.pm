# Adapted from Perl 5.26's lib/B/Deparse-core.t
1;
__DATA__
#
# format:
#   keyword args flags
#
# args consists of:
#  * one of more digits indictating which lengths of args the function accepts,
#  * or 'B' to indiate a binary infix operator,
#  * or '@' to indicate a list function.
#
# Flags consists of the following (or '-' if no flags):
#    + : strong keyword: can't be overrriden
#    p : the args are parenthesised on deparsing;
#    1 : parenthesising of 1st arg length is inverted
#        so '234 p1' means: foo a1,a2;  foo(a1,a2,a3); foo(a1,a2,a3,a4)
#    $ : on the first argument length, there is an implicit extra
#        '$_' arg which will appear on deparsing;
#        e.g. 12p$  will be tested as: foo(a1);     foo(a1,a2);
#                     and deparsed as: foo(a1, $_); foo(a1,a2);
#
# XXX Note that we really should get this data from regen/keywords.pl
# and regen/opcodes (augmented if necessary), rather than duplicating it
# here.

__SUB__          0     -
accept           2     p
and              B     -
atan2            2     p
bind             2     p
binmode          12    p
bless            1     p
chown            @     p1
cmp              B     -
connect          2     p
crypt            2     p
# dbmopen  handled specially
# dbmclose handled specially
# delete handled specially
die              @     p1
# do handled specially
# dump handled specially
# each handled specially
endservent       0     -
eq               B     -
eval             01    $+
evalbytes        01    $
# exists handled specially
exp              01    $
fcntl            3     p
flock            2     p
formline         2     p
ge               B     -
gethostbyaddr    2     p
getnetbyaddr     2     p
getpriority      2     p
getprotobynumber 1     p
getservbyname    2     p
getservbyport    2     p
getsockopt       3     p
# given handled specially
# grep             123   p+ # also tested specially
# glob handled specially
# goto handled specially
gt               B     -
index            23    p
ioctl            3     p
join             13    p
# keys handled specially
kill             123   p
# last handled specially
lc               01    $
lcfirst          01    $
le               B     -
link             2     p
listen           2     p
local            1     p+
lstat            01    $
lt               B     -
# map              123   p+ # also tested specially
mkdir            @     p$
msgctl           3     p
msgget           2     p
msgrcv           5     p
msgsnd           3     p
my               123   p+ # skip with 0 args, as my() => ()
ne               B     -
# next handled specially
# not handled specially
open             12345 p
opendir          2     p
or               B     -
our              123   p+ # skip with 0 args, as our() => ()
pack             123   p
pipe             2     p
# print            @     p$+
# printf           @     p$+
# push handled specially
quotemeta        01    $
read             34    p
# readline handled specially
# readpipe handled specially
recv             4     p
# redo handled specially
rename           2     p
# XXX This code prints 'Undefined subroutine &main::require called':
#   use subs (); import subs 'require';
#   eval q[no strict 'vars'; sub { () = require; }]; print $@;
# so disable for now
#require          01    $+
# return handled specially
reverse          @     p1 # also tested specially
rindex           23    p
# our setp erroneously adds $_
# say              @     p$+
seek             3     p
seekdir          2     p
select           014   p1
semctl           4     p
semget           3     p
semop            2     p
send             34    p
setgrent         0     -
setpgrp          2     p
setpriority      3     p
setsockopt       4     p
shmctl           3     p
shmget           3     p
shmread          4     p
shmwrite         4     p
shutdown         2     p
socket           4     p
socketpair       5     p
# sort             @     p1+
# split handled specially
# splice handled specially
sprintf          123   p
stat             01    $
state            123   p+ # skip with 0 args, as state() => ()
# sub handled specially
substr           234   p
symlink          2     p
syscall          2     p
sysopen          34    p
sysread          34    p
sysseek          3     p
system           @     p1 # also tested specially
syswrite         234   p
tie              234   p
truncate         2     p
uc               01    $
ucfirst          01    $
unlink           @     p$
unpack           12    p$
# unshift handled specially
utime            @     p1
# values handled specially
vec              3     p
waitpid          2     p
wantarray        0     -
warn             @     p1
x                B     -
xor              B     -
