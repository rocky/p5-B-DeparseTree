# Common PP opcodes. Specifc Perl versions can override these.

# Copyright (c) 2015, 2018 Rocky Bernstein
#
use strict;
use warnings ();
require feature;

my %feature_keywords = (
  # keyword => 'feature',
    state   => 'state',
    say     => 'say',
    given   => 'switch',
    when    => 'switch',
    default => 'switch',
    break   => 'switch',
    evalbytes=>'evalbytes',
    __SUB__ => '__SUB__',
   fc       => 'fc',
);

use rlib '../..';

package B::DeparseTree::PP;

use B::DeparseTree::Common;
use B::DeparseTree::PPfns;
use B::DeparseTree::Node;
use B::Deparse;

*is_ifelse_cont = *B::Deparse::is_ifelse_cont;
*real_negate = *B::Deparse::real_negate;
*pp_anonhash = *B::Deparse::pp_anonhash;
*pp_anonlist = *B::Deparse::pp_anonlist;
*pp_negate = *B::Deparse::pp_negate;
*pp_i_negate = *B::Deparse::pp_i_negate;

use B qw(
    class
    OPf_MOD OPpENTERSUB_AMPER
    OPf_SPECIAL
    OPf_STACKED
    OPpEXISTS_SUB
    SVf_POK
    SVf_ROK
);

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.0.0';

@ISA = qw(Exporter B::Deparse);
@EXPORT = qw(
    pp_aassign
    pp_accept
    pp_abs
    pp_and
    pp_anonhash
    pp_anonlist
    pp_atan2
    pp_bind
    pp_binmode
    pp_bless
    pp_chr
    pp_chmod
    pp_chomp
    pp_chop
    pp_chown
    pp_clonecv
    pp_complement
    pp_cond_expr
    pp_connect
    pp_const
    pp_cos
    pp_crypt
    pp_dbmopen
    pp_dbstate
    pp_defined
    pp_delete
    pp_die
    pp_dor
    pp_each
    pp_egrent
    pp_ehostent
    pp_enetent
    pp_entersub
    pp_eprotoent pp_epwent pp_eservent
    pp_exec
    pp_exists
    pp_exp
    pp_fcntl
    pp_flock
    pp_fork pp_getlogin pp_ggrent
    pp_formline
    pp_getppid
    pp_getpriority
    pp_ghbyaddr
    pp_ghostent pp_gnetent pp_gprotoent
    pp_glob
    pp_gnbyaddr
    pp_gpbynumber
    pp_gpwent pp_grepstart pp_gservent
    pp_grepwhile
    pp_gsbyname
    pp_gsbyport
    pp_gsockopt
    pp_gv
    pp_hex
    pp_i_negate
    pp_i_predec
    pp_i_preinc
    pp_index
    pp_int
    pp_introcv
    pp_ioctl
    pp_join
    pp_keys
    pp_kill
    pp_leave
    pp_length
    pp_leaveloop
    pp_leavetry
    pp_lineseq
    pp_link
    pp_list
    pp_listen
    pp_log
    pp_mapstart
    pp_mapwhile
    pp_mkdir
    pp_msgctl
    pp_msgget
    pp_msgrcv
    pp_msgsnd
    pp_negate
    pp_nextstate
    pp_null
    pp_oct
    pp_once
    pp_open
    pp_open_dir
    pp_or
    pp_ord
    pp_pos
    pp_pack
    pp_padcv
    pp_pipe_op
    pp_pos
    pp_postdec
    pp_postinc
    pp_predec
    pp_preinc
    pp_print
    pp_prtf
    pp_push
    pp_rand
    pp_read
    pp_recv
    pp_ref
    pp_refgen
    pp_rename
    pp_repeat
    pp_require
    pp_return
    pp_reverse
    pp_rindex
    pp_say
    pp_schomp
    pp_schop
    pp_scope
    pp_seek
    pp_seekdir
    pp_select
    pp_semctl
    pp_semget
    pp_semop
    pp_send
    pp_setpgrp
    pp_setpriority
    pp_setstate
    pp_sgrent
    pp_shmctl
    pp_shmget
    pp_shmread
    pp_shmwrite
    pp_shutdown
    pp_sin
    pp_socket
    pp_sockpair
    pp_sort
    pp_splice
    pp_sprintf
    pp_spwent
    pp_sqrt
    pp_srand
    pp_srefgen
    pp_sselect
    pp_ssockopt
    pp_stub
    pp_study
    pp_substr
    pp_symlink
    pp_symlink
    pp_syscall
    pp_sysopen
    pp_sysread
    pp_sysseek
    pp_system
    pp_syswrite
    pp_tie
    pp_time
    pp_tms
    pp_undef
    pp_unlink
    pp_unpack
    pp_unshift
    pp_unstack
    pp_utime
    pp_vec
    pp_values
    pp_wait
    pp_waitpid
    pp_wantarray
    pp_warn
    pp_xor
    );

BEGIN {
    # List version-specific constants here.
    # Easiest way to keep this code portable between version looks to
    # be to fake up a dummy constant that will never actually be true.
    foreach (qw(OPpCONST_ARYBASE OPpEVAL_BYTES)) {
	eval { import B $_ };
	no strict 'refs';
	*{$_} = sub () {0} unless *{$_}{CODE};
    }
}

sub SWAP_CHILDREN () { 1 }
sub ASSIGN () { 2 } # has OP= variant
sub LIST_CONTEXT () { 4 } # Assignment is in list context

sub pp_aassign { binop(@_, "=", 7, SWAP_CHILDREN | LIST_CONTEXT, 'array assign') }
sub pp_abs { maybe_targmy(@_, \&unop, "abs") }
sub pp_accept { listop(@_, "accept") }
sub pp_atan2 { maybe_targmy(@_, \&listop, "atan2") }
sub pp_bind { listop(@_, "bind") }
sub pp_binmode { listop(@_, "binmode") }
sub pp_bless { listop(@_, "bless") }
sub pp_chmod { maybe_targmy(@_, \&listop, "chmod") }
sub pp_chown { maybe_targmy(@_, \&listop, "chown") }
sub pp_chr { maybe_targmy(@_, \&unop, "chr") }
sub pp_connect { listop(@_, "connect") }
sub pp_cos { maybe_targmy(@_, \&unop, "cos") }
sub pp_crypt { maybe_targmy(@_, \&listop, "crypt") }
sub pp_dbmopen { listop(@_, "dbmopen") }
sub pp_die { listop(@_, "die") }
sub pp_each { unop(@_, "each") }
sub pp_exec { maybe_targmy(@_, \&listop, "exec") }
sub pp_exp { maybe_targmy(@_, \&unop, "exp") }
sub pp_fcntl { listop(@_, "fcntl") }
sub pp_flock { maybe_targmy(@_, \&listop, "flock") }
sub pp_formline { listop(@_, "formline") } # see also deparse_format
sub pp_getpriority { maybe_targmy(@_, \&listop, "getpriority") }
sub pp_ghbyaddr { listop(@_, "gethostbyaddr") }
sub pp_gnbyaddr { listop(@_, "getnetbyaddr") }
sub pp_gpbynumber { listop(@_, "getprotobynumber") }
sub pp_gsbyname { listop(@_, "getservbyname") }
sub pp_gsbyport { listop(@_, "getservbyport") }
sub pp_gsockopt { listop(@_, "getsockopt") }
sub pp_hex { maybe_targmy(@_, \&unop, "hex") }
sub pp_index { maybe_targmy(@_, \&listop, "index") }
sub pp_int { maybe_targmy(@_, \&unop, "int") }
sub pp_ioctl { listop(@_, "ioctl") }
sub pp_join { maybe_targmy(@_, \&listop, "join") }
sub pp_keys { unop(@_, "keys") }
sub pp_kill { maybe_targmy(@_, \&listop, "kill") }
sub pp_length { maybe_targmy(@_, \&unop, "length") }
sub pp_link { maybe_targmy(@_, \&listop, "link") }
sub pp_listen { listop(@_, "listen") }
sub pp_log { maybe_targmy(@_, \&unop, "log") }
sub pp_mkdir { maybe_targmy(@_, \&listop, "mkdir") }
sub pp_msgctl { listop(@_, "msgctl") }
sub pp_msgget { listop(@_, "msgget") }
sub pp_msgrcv { listop(@_, "msgrcv") }
sub pp_msgsnd { listop(@_, "msgsnd") }
sub pp_oct { maybe_targmy(@_, \&unop, "oct") }
sub pp_open { listop(@_, "open") }
sub pp_open_dir { listop(@_, "opendir") }
sub pp_ord { maybe_targmy(@_, \&unop, "ord") }
sub pp_pack { listop(@_, "pack") }
sub pp_pipe_op { listop(@_, "pipe") }
sub pp_pos { maybe_local(@_, unop(@_, "pos")) }
sub pp_push { maybe_targmy(@_, \&listop, "push") }
sub pp_read { listop(@_, "read") }
sub pp_recv { listop(@_, "recv") }
sub pp_rename { maybe_targmy(@_, \&listop, "rename") }
sub pp_return { listop(@_, "return", undef, 1) } # llafr does not apply
sub pp_reverse { listop(@_, "reverse") }
sub pp_rindex { maybe_targmy(@_, \&listop, "rindex") }
sub pp_seek { listop(@_, "seek") }
sub pp_seekdir { listop(@_, "seekdir") }
sub pp_select { listop(@_, "select") }
sub pp_semctl { listop(@_, "semctl") }
sub pp_semget { listop(@_, "semget") }
sub pp_semop { listop(@_, "semop") }
sub pp_send { listop(@_, "send") }
sub pp_setpgrp { maybe_targmy(@_, \&listop, "setpgrp") }
sub pp_setpriority { maybe_targmy(@_, \&listop, "setpriority") }
sub pp_shmctl { listop(@_, "shmctl") }
sub pp_shmget { listop(@_, "shmget") }
sub pp_shmread { listop(@_, "shmread") }
sub pp_shmwrite { listop(@_, "shmwrite") }
sub pp_shutdown { listop(@_, "shutdown") }
sub pp_sin { maybe_targmy(@_, \&unop, "sin") }
sub pp_socket { listop(@_, "socket") }
sub pp_sockpair { listop(@_, "socketpair") }
sub pp_splice { listop(@_, "splice") }
sub pp_sprintf { maybe_targmy(@_, \&listop, "sprintf") }
sub pp_sqrt { maybe_targmy(@_, \&unop, "sqrt") }
sub pp_sselect { listop(@_, "select") }
sub pp_ssockopt { listop(@_, "setsockopt") }
sub pp_symlink { maybe_targmy(@_, \&listop, "symlink") }
sub pp_syscall { listop(@_, "syscall") }
sub pp_sysopen { listop(@_, "sysopen") }
sub pp_sysread { listop(@_, "sysread") }
sub pp_sysseek { listop(@_, "sysseek") }
sub pp_system { maybe_targmy(@_, \&listop, "system") }
sub pp_syswrite { listop(@_, "syswrite") }
sub pp_tie { listop(@_, "tie") }
sub pp_unlink { maybe_targmy(@_, \&listop, "unlink") }
sub pp_unpack { listop(@_, "unpack") }
sub pp_unshift { maybe_targmy(@_, \&listop, "unshift") }
sub pp_utime { maybe_targmy(@_, \&listop, "utime") }
sub pp_values { unop(@_, "values") }
sub pp_vec { maybe_local(@_, listop(@_, "vec")) }
sub pp_waitpid { maybe_targmy(@_, \&listop, "waitpid") }
sub pp_warn { listop(@_, "warn") }

sub pp_glob
{
    my($self, $op, $cx) = @_;

    my $opts = {other_ops => [$op->first]};
    my $kid = $op->first->sibling;  # skip pushmark
    my $keyword =
	$op->flags & OPf_SPECIAL ? 'glob' : $self->keyword('glob');

    if ($keyword =~ /^CORE::/ or $kid->name ne 'const') {
	my $kid_info = $self->dq($kid, $op);
	my $body = [$kid_info];
	my $text = $kid_info->{text};
	if ($text =~ /^\$?(\w|::|\`)+$/ # could look like a readline
	    or $text =~ /[<>]/) {
	    $kid_info = $self->deparse($kid, 0, $op);
	    $body = [$kid_info];
	    $text = $kid_info->{text};
	    $opts->{body} = $body;
	    if ($cx >= 5 || $self->{'parens'}) {
		return info_from_list($op, $self, [$keyword, '(', $text, ')'], '',
				      'glob_paren', $opts);
	    } else {
		return info_from_list($op, $self, [$keyword, $text], ' ',
				      'glob_space', $opts);
	    }
	} else {
	    return info_from_list($op, $self, ['<', $text, '>'], '', 'glob_angle', $opts);
	}
    }
    return info_from_list($op, $self, ['<', '>'], '', 'glob_angle', $opts);
}

sub pp_chomp { maybe_targmy(@_, \&unop, "chomp") }
sub pp_chop { maybe_targmy(@_, \&unop, "chop") }

sub pp_clonecv {
    my $self = shift;
    my($op, $cx) = @_;
    my $sv = $self->padname_sv($op->targ);
    my $name = substr $sv->PVX, 1; # skip &/$/@/%, like $self->padany
    return info_from_list($op, $self, ['my', 'sub', $name], ' ', 'clonev', {});
}

sub pp_defined { unop(@_, "defined") }

sub pp_delete($$$)
{
    my($self, $op, $cx) = @_;
    my $arg;
    my ($info, $body, $type);
    if ($op->private & B::OPpSLICE) {
	if ($op->flags & B::OPf_SPECIAL) {
	    # Deleting from an array, not a hash
	    $info = $self->pp_aslice($op->first, 16);
	    $type = 'delete slice';
	}
    } else {
	if ($op->flags & B::OPf_SPECIAL) {
	    # Deleting from an array, not a hash
	    $info = $self->pp_aelem($op->first, 16);
	    $type = 'delete array'
	} else {
	    $info = $self->pp_helem($op->first, 16);
	    $type = 'delete hash';
	}
    }
    my @texts = $self->maybe_parens_func("delete",
					 $info->{text}, $cx, 16);
    return info_from_list($op, $self, \@texts, '', $type, {body => [$info]});
}


sub pp_egrent { baseop(@_, "endgrent") }
sub pp_ehostent { baseop(@_, "endhostent") }
sub pp_enetent { baseop(@_, "endnetent") }
sub pp_eprotoent { baseop(@_, "endprotoent") }
sub pp_epwent { baseop(@_, "endpwent") }
sub pp_eservent { baseop(@_, "endservent") }

sub pp_exists
{
    my($self, $op, $cx) = @_;
    my ($info, $type);
    my $name = $self->keyword("exists");
    if ($op->private & OPpEXISTS_SUB) {
	# Checking for the existence of a subroutine
	$info = $self->pp_rv2cv($op->first, 16);
	$type = 'exists sub';
    } elsif ($op->flags & OPf_SPECIAL) {
	# Array element, not hash helement
	$info = $self->pp_aelem($op->first, 16);
	$type = 'exists array';
    } else {
	$info = $self->pp_helem($op->first, 16);
	$type = 'exists hash';
    }
    my @texts = $self->maybe_parens_func($name, $info->{text}, $cx, 16);
    return info_from_list($op, $self, \@texts, '', $type, {});
}

sub pp_fork { baseop(@_, "fork") }
sub pp_getlogin { baseop(@_, "getlogin") }
sub pp_ggrent { baseop(@_, "getgrent") }
sub pp_ghostent { baseop(@_, "gethostent") }
sub pp_gnetent { baseop(@_, "getnetent") }
sub pp_gprotoent { baseop(@_, "getprotoent") }
sub pp_gpwent { baseop(@_, "getpwent") }
sub pp_grepstart { baseop(@_, "grep") }
sub pp_gservent { baseop(@_, "getservent") }

sub pp_introcv
{
    my($self, $op, $cx) = @_;
    # For now, deparsing doesn't worry about the distinction between introcv
    # and clonecv, so pretend this op doesn't exist:
    return info_from_text($op, $self, '', 'introcv', {});
}

sub pp_leave { scopeop(1, @_); }
sub pp_leaveloop { shift->loop_common(@_, undef); }

sub pp_leavetry {
    my ($self, $op, $cx) = @_;
    my $leave_info = $self->pp_leave($op, $cx);
    return $self->info_from_template('leavetry', $op, "eval {\n%+%c\n%-}",
				     [0], [$leave_info]);
}

sub pp_lineseq { scopeop(0, @_); }

sub pp_list
{
    my($self, $op, $cx) = @_;
    my($expr, @exprs);

    my $other_op = $op->first;
    my $kid = $op->first->sibling; # skip a pushmark

    if (class($kid) eq 'NULL') {
	return info_from_text($op, $self, '', 'list_null',
			      {other_ops => [$other_op]});
    }
    my $lop;
    my $local = "either"; # could be local(...), my(...), state(...) or our(...)
    for ($lop = $kid; !null($lop); $lop = $lop->sibling) {
	# This assumes that no other private flags equal 128, and that
	# OPs that store things other than flags in their op_private,
	# like OP_AELEMFAST, won't be immediate children of a list.
	#
	# OP_ENTERSUB and OP_SPLIT can break this logic, so check for them.
	# I suspect that open and exit can too.
	# XXX This really needs to be rewritten to accept only those ops
	#     known to take the OPpLVAL_INTRO flag.

	if (!($lop->private & (B::Deparse::OPpLVAL_INTRO|B::Deparse::OPpOUR_INTRO)
		or $lop->name eq "undef")
	    or $lop->name =~ /^(?:entersub|exit|open|split)\z/)
	{
	    $local = ""; # or not
	    last;
	}
	if ($lop->name =~ /^pad[ash]v$/) {
	    if ($lop->private & B::Deparse::OPpPAD_STATE) { # state()
		($local = "", last) if $local =~ /^(?:local|our|my)$/;
		$local = "state";
	    } else { # my()
		($local = "", last) if $local =~ /^(?:local|our|state)$/;
		$local = "my";
	    }
	} elsif ($lop->name =~ /^(gv|rv2)[ash]v$/
			&& $lop->private & B::Deparse::OPpOUR_INTRO
		or $lop->name eq "null" && $lop->first->name eq "gvsv"
			&& $lop->first->private & B::Deparse::OPpOUR_INTRO) { # our()
	    ($local = "", last) if $local =~ /^(?:my|local|state)$/;
	    $local = "our";
	} elsif ($lop->name ne "undef"
		# specifically avoid the "reverse sort" optimisation,
		# where "reverse" is nullified
		&& !($lop->name eq 'sort' && ($lop->flags & B::Deparse::OPpSORT_REVERSE)))
	{
	    # local()
	    ($local = "", last) if $local =~ /^(?:my|our|state)$/;
	    $local = "local";
	}
    }
    $local = "" if $local eq "either"; # no point if it's all undefs
    if (B::Deparse::null $kid->sibling and not $local) {
	my $info = $self->deparse($kid, $cx, $op);
	my @other_ops = $info->{other_ops} ? @{$info->{other_ops}} : ();
	$info->{other_ops} = \@other_ops;
	return $info;
    }

    for (; !null($kid); $kid = $kid->sibling) {
	if ($local) {
	    if (class($kid) eq "UNOP" and $kid->first->name eq "gvsv") {
		$lop = $kid->first;
	    } else {
		$lop = $kid;
	    }
	    $self->{'avoid_local'}{$$lop}++;
	    $expr = $self->deparse($kid, 6, $op);
	    delete $self->{'avoid_local'}{$$lop};
	} else {
	    $expr = $self->deparse($kid, 6, $op);
	}
	push @exprs, $expr;
    }

    if ($local) {
	return $self->info_from_template("$local list", $op,
					 "$local(%C)", [[0, $#exprs, ', ']],
					 \@exprs);

    } else {
	return $self->info_from_template("list", $op,
					 "%C", [[0, $#exprs, ', ']],
					 \@exprs,
					 {maybe_parens => [$self, $cx, 6]});
    }
}


sub pp_mapstart { baseop(@_, "map") }

sub pp_padcv {
    my($self, $op, $cx) = @_;
    return info_from_text($op, $self, $self->padany($op), 'padcv', {});
}

sub pp_ref { unop(@_, "ref") }

sub pp_refgen
{
    my($self, $op, $cx) = @_;
    my $kid = $op->first;
    if ($kid->name eq "null") {
	my $other_ops = [$kid];
	my $anoncode = $kid = $kid->first;
	if ($anoncode->name eq "anonconst") {
	    $anoncode = $anoncode->first->first->sibling;
	}
	if ($anoncode->name eq "anoncode"
	 or !null($anoncode = $kid->sibling) and
		 $anoncode->name eq "anoncode") {
            return $self->e_anoncode({ code => $self->padval($anoncode->targ) });
	} elsif ($kid->name eq "pushmark") {
            my $sib_name = $kid->sibling->name;
            if ($sib_name =~ /^enter(xs)?sub/) {
                my $kid_info = $self->deparse($kid->sibling, 1, $op);
                # Always show parens for \(&func()), but only with -p otherwise
		my @texts = ('\\', $kid_info->{text});
		if ($self->{'parens'} or $kid->sibling->private & OPpENTERSUB_AMPER) {
		    @texts = ('(', "\\", $kid_info->{text}, ')');
		}
		return info_from_list($op, $self, \@texts, '', 'refgen_entersub',
				      {body => [$kid_info],
				       other_ops => $other_ops});
            }
        }
    }
    local $self->{'in_refgen'} = 1;
    $self->pfixop($op, $cx, "\\", 20);
}

sub pp_require
{
    my($self, $op, $cx) = @_;
    my $opname = $op->flags & OPf_SPECIAL ? 'CORE::require' : 'require';
    if (class($op) eq "UNOP" and $op->first->name eq "const"
	and $op->first->private & B::OPpCONST_BARE) {
	my $name = $self->const_sv($op->first)->PV;
	$name =~ s[/][::]g;
	$name =~ s/\.pm//g;
	return info_from_list($op, $self, [$opname, $name], ' ',
			      'require',
			      {maybe_parens => [$self, $cx, 16]});
    } else {
	return $self->unop(
	    $op, $cx,
	    $op->first->name eq 'const'
	    && $op->first->private & B::OPpCONST_NOVER
	    ? "no"
	    : $opname,
	    1, # llafr does not apply
	    );
    }
    Carp::confess("unhandled condition in pp_require");
}


sub pp_schomp { maybe_targmy(@_, \&unop, "chomp") }
sub pp_schop { maybe_targmy(@_, \&unop, "chop") }
sub pp_scope { scopeop(0, @_); }
sub pp_sgrent { baseop(@_, "setgrent") }
sub pp_spwent { baseop(@_, "setpwent") }
sub pp_srand { unop(@_, "srand") }
sub pp_srefgen { pp_refgen(@_) }
sub pp_study { unop(@_, "study") }
sub pp_tms { baseop(@_, "times") }

my $count = 0;
# Notice how subs and formats are inserted between statements here;
# also $[ assignments and pragmas.
sub pp_nextstate
{
    my $self = shift;
    my($op, $cx) = @_;
    $self->{'curcop'} = $op;
    my @texts;
    my $opts = {};
    my @args_spec = ();
    my $fmt = '%;';

    push @texts, $self->cop_subs($op);
    if (@texts) {
	# Special marker to swallow up the semicolon
	$opts->{'omit_next_semicolon'} = 1;
    }

    my $stash = $op->stashpv;
    if ($stash ne $self->{'curstash'}) {
	push @texts, $self->keyword("package") . " $stash;";
	$self->{'curstash'} = $stash;
    }

    if (OPpCONST_ARYBASE && $self->{'arybase'} != $op->arybase) {
	push @texts, '$[ = '. $op->arybase .";";
	$self->{'arybase'} = $op->arybase;
    }

    my $warnings = $op->warnings;
    my $warning_bits;
    if ($warnings->isa("B::SPECIAL") && $$warnings == 4) {
	$warning_bits = $warnings::Bits{"all"} & WARN_MASK;
    }
    elsif ($warnings->isa("B::SPECIAL") && $$warnings == 5) {
        $warning_bits = $warnings::NONE;
    }
    elsif ($warnings->isa("B::SPECIAL")) {
	$warning_bits = undef;
    }
    else {
	$warning_bits = $warnings->PV & WARN_MASK;
    }

    if (defined ($warning_bits) and
       !defined($self->{warnings}) || $self->{'warnings'} ne $warning_bits) {
	my @warnings = $self->declare_warnings($self->{'warnings'}, $warning_bits);
	foreach my $warning (@warnings) {
	    push @texts, $warning;
	}
    	$self->{'warnings'} = $warning_bits;
    }

    my $hints = $] < 5.008009 ? $op->private : $op->hints;
    my $old_hints = $self->{'hints'};
    if ($self->{'hints'} != $hints) {
	my @hints = $self->declare_hints($self->{'hints'}, $hints);
	foreach my $hint (@hints) {
	    push @texts, $hint;
	}
	$self->{'hints'} = $hints;
    }

    my $newhh;
    if ($] > 5.009) {
	$newhh = $op->hints_hash->HASH;
    }

    if ($] >= 5.015006) {
	# feature bundle hints
	my $from = $old_hints & $feature::hint_mask;
	my $to   = $    hints & $feature::hint_mask;
	if ($from != $to) {
	    if ($to == $feature::hint_mask) {
		if ($self->{'hinthash'}) {
		    delete $self->{'hinthash'}{$_}
			for grep /^feature_/, keys %{$self->{'hinthash'}};
		}
		else { $self->{'hinthash'} = {} }
		$self->{'hinthash'}
		    = _features_from_bundle($from, $self->{'hinthash'});
	    }
	    else {
		my $bundle =
		    $feature::hint_bundles[$to >> $feature::hint_shift];
		$bundle =~ s/(\d[13579])\z/$1+1/e; # 5.11 => 5.12
		push @texts,
		    $self->keyword("no") . " feature ':all'",
		    $self->keyword("use") . " feature ':$bundle'";
	    }
	}
    }

    if ($] > 5.009) {
	# FIXME use format specifiers
	my @hints = $self->declare_hinthash(
	    $self->{'hinthash'}, $newhh, 0, $self->{hints});
	foreach my $hint (@hints) {
	    push @texts, $hint;
	}
	$self->{'hinthash'} = $newhh;
    }


    # This should go after of any branches that add statements, to
    # increase the chances that it refers to the same line it did in
    # the original program.
    if ($self->{'linenums'} && $cx != .5) { # $cx == .5 means in a format
	my $line = sprintf("\n# line %s '%s'", $op->line, $op->file);
	$line .= sprintf(" 0x%x", $$op) if $self->{'opaddr'};
	$opts->{'omit_next_semicolon'} = 1;
	push @texts, $line;
    }

    if ($op->label) {
	$fmt .= "%c\n";
	push @args_spec, scalar(@args_spec);
	push @texts, $op->label . ": " ;
    }

    return $self->info_from_template("nextstate", $op, $fmt,
				     \@args_spec, \@texts, $opts);
}

sub pp_and { logop(@_, "and", 3, "&&", 11, "if") }

sub pp_cond_expr
{
    my $self = shift;
    my($op, $cx) = @_;
    my $cond = $op->first;
    my $true = $cond->sibling;
    my $false = $true->sibling;
    my $cuddle = $self->{'cuddle'};
    my $type = 'if';
    unless ($cx < 1 and (is_scope($true) and $true->name ne "null") and
	    (is_scope($false) || is_ifelse_cont($false))
	    and $self->{'expand'} < 7) {
	# FIXME: turn into template
	my $cond_info = $self->deparse($cond, 8, $op);
	my $true_info = $self->deparse($true, 6, $op);
	my $false_info = $self->deparse($false, 8, $op);
	return $self->info_from_template('ternary ?', $op, "%c ? %c : %c",
					 [0, 1, 2],
					 [$cond_info, $true_info, $false_info],
					 {maybe_parens => [$self, $cx, 8]});
    }

    my $cond_info = $self->deparse($cond, 1, $op);
    my $true_info = $self->deparse($true, 0, $op);
    my $fmt = "%|if (%c) {\n%+%c\n%-}";
    my @exprs = ($cond_info, $true_info);
    my @args_spec = (0, 1);

    my $i;
    for ($i=0; !null($false) and is_ifelse_cont($false); $i++) {
	my $newop = $false->first;
	my $newcond = $newop->first;
	my $newtrue = $newcond->sibling;
	$false = $newtrue->sibling; # last in chain is OP_AND => no else
	if ($newcond->name eq "lineseq")
	{
	    # lineseq to ensure correct line numbers in elsif()
	    # Bug #37302 fixed by change #33710.
	    $newcond = $newcond->first->sibling;
	}
	my $newcond_info = $self->deparse($newcond, 1, $op);
	my $newtrue_info = $self->deparse($newtrue, 0, $op);
	push @args_spec, scalar(@args_spec), scalar(@args_spec)+1;
	push @exprs, $newcond_info, $newtrue_info;
	$fmt .= " elsif ( %c ) {\n%+%c\n\%-}";
    }
    $type .= " elsif($i)" if $i;
    my $false_info;
    if (!null($false)) {
	$false_info = $self->deparse($false, 0, $op);
	$fmt .= "${cuddle}else {\n%+%c\n%-}";
	push @args_spec, scalar(@args_spec);
	push @exprs, $false_info;
	$type .= ' else';
    }
    return $self->info_from_template($type, $op, $fmt, \@args_spec, \@exprs);
}

sub pp_const {
    my $self = shift;
    my($op, $cx) = @_;
    if ($op->private & OPpCONST_ARYBASE) {
        return info_from_text($op, $self, '$[', 'const_ary', {});
    }
    # if ($op->private & OPpCONST_BARE) { # trouble with '=>' autoquoting
    # 	return $self->const_sv($op)->PV;
    # }
    my $sv = $self->const_sv($op);
    return $self->const($sv, $cx);;
}

sub pp_dbstate { pp_nextstate(@_) }

sub pp_entersub
{
    my($self, $op, $cx) = @_;
    return $self->e_method($op, $self->_method($op, $cx))
        unless null $op->first->sibling;
    my $prefix = "";
    my $amper = "";
    my($kid, @exprs);
    if ($op->flags & OPf_SPECIAL && !($op->flags & OPf_MOD)) {
	$prefix = "do ";
    } elsif ($op->private & OPpENTERSUB_AMPER) {
	$amper = "&";
    }
    $kid = $op->first;

    my $other_ops = [$kid, $kid->first];
    $kid = $kid->first->sibling; # skip ex-list, pushmark

    for (; not null $kid->sibling; $kid = $kid->sibling) {
	push @exprs, $kid;
    }
    my ($simple, $proto, $subname_info) = (0, undef, undef);
    if (is_scope($kid)) {
	$amper = "&";
	$subname_info = $self->deparse($kid, 0, $op);
	$subname_info->{texts} = ['{', $subname_info->texts, '}'];
	$subname_info->{text} = join('', @$subname_info->{texts});
    } elsif ($kid->first->name eq "gv") {
	my $gv = $self->gv_or_padgv($kid->first);
	my $cv;
	if (class($gv) eq 'GV' && class($cv = $gv->CV) ne "SPECIAL"
	 || $gv->FLAGS & SVf_ROK && class($cv = $gv->RV) eq 'CV') {
	    $proto = $cv->PV if $cv->FLAGS & SVf_POK;
	}
	$simple = 1; # only calls of named functions can be prototyped
	$subname_info = $self->deparse($kid, 24, $op);
	my $fq;
	# Fully qualify any sub name that conflicts with a lexical.
	if ($self->lex_in_scope("&$kid")
	 || $self->lex_in_scope("&$kid", 1))
	{
	    $fq++;
	} elsif (!$amper) {
	    if ($subname_info->{text} eq 'main::') {
		$subname_info->{text} = '::';
	    } else {
	      if ($kid !~ /::/ && $kid ne 'x') {
		# Fully qualify any sub name that is also a keyword.  While
		# we could check the import flag, we cannot guarantee that
		# the code deparsed so far would set that flag, so we qual-
		# ify the names regardless of importation.
		if (exists $feature_keywords{$kid}) {
		    $fq++ if $self->feature_enabled($kid);
		} elsif (do { local $@; local $SIG{__DIE__};
			      eval { () = prototype "CORE::$kid"; 1 } }) {
		    $fq++
		}
	      }
	    }
	    if ($subname_info->{text} !~ /^(?:\w|::)(?:[\w\d]|::(?!\z))*\z/) {
		$subname_info->{text} = $self->single_delim($$kid, "q", "'", $kid) . '->';
	    }
	}
    } elsif (is_scalar ($kid->first) && $kid->first->name ne 'rv2cv') {
	$amper = "&";
	$subname_info = $self->deparse($kid, 24, $op);
    } else {
	$prefix = "";
	my $arrow = is_subscriptable($kid->first) || $kid->first->name eq "padcv" ? "" : "->";
	$subname_info = $self->deparse($kid, 24, $op);
	$subname_info->{text} .= $arrow;
    }

    # Doesn't matter how many prototypes there are, if
    # they haven't happened yet!
    my $declared;
    my $sub_name = $subname_info->{text};
    {
	no strict 'refs';
	no warnings 'uninitialized';
	$declared = exists $self->{'subs_declared'}{$sub_name}
	    || (
		 defined &{ ${$self->{'curstash'}."::"}{$sub_name} }
		 && !exists
		     $self->{'subs_deparsed'}{$self->{'curstash'}."::" . $sub_name}
		 && defined prototype $self->{'curstash'}."::" . $sub_name
	       );
	if (!$declared && defined($proto)) {
	    # Avoid "too early to check prototype" warning
	    ($amper, $proto) = ('&');
	}
    }

    my (@texts, @body, $type);
    @body = ();
    if ($declared and defined $proto and not $amper) {
	my $args;
	($amper, $args) = $self->check_proto($op, $proto, @exprs);
	if ($amper eq "&") {
	    @body = map($self->deparse($_, 6, $op), @exprs);
	} else {
	    @body = @$args if @$args;
	}
    } else {
	@body  = map($self->deparse($_, 6, $op), @exprs);
    }

    if ($prefix or $amper) {
	if ($sub_name eq '&') {
	    # &{&} cannot be written as &&
	    $subname_info->{texts} = ["{", @{$subname_info->{texts}}, "}"];
	    $subname_info->{text} = join('', $subname_info->{texts});
	}
	if ($op->flags & OPf_STACKED) {
	    $type = 'prefix- or &-stacked call()';
	    @texts = ($prefix, $amper, $subname_info, "(", $self->combine2str(', ', \@body), ")");
	} else {
	    $type = 'prefix or &- call';
	    @texts = ($prefix, $amper, $subname_info);
	}
    } else {
	# It's a syntax error to call CORE::GLOBAL::foo with a prefix,
	# so it must have been translated from a keyword call. Translate
	# it back.
	$subname_info->{text} =~ s/^CORE::GLOBAL:://;
	my $dproto = defined($proto) ? $proto : "undefined";
        if (!$declared) {
	    $type = 'undeclared call';
	    @texts = dedup_parens_func($self, $subname_info, \@body);
	    return B::DeparseTree::Node->new($op, $self, \@texts,
					     '', $type,
					     {other_ops => $other_ops});
	} elsif ($dproto =~ /^\s*\z/) {
	    $type = 'call no protype';
	    @texts = ($subname_info);
	} elsif ($dproto eq "\$" and is_scalar($exprs[0])) {
	    $type = 'call - $ prototype';
	    # is_scalar is an excessively conservative test here:
	    # really, we should be comparing to the precedence of the
	    # top operator of $exprs[0] (ala unop()), but that would
	    # take some major code restructuring to do right.
	    @texts = $self->maybe_parens_func($sub_name, $self->combine2str(', ', \@body), $cx, 16);
	} elsif ($dproto ne '$' and defined($proto) || $simple) { #'
	    $type = 'call with prototype';
	    @texts = $self->maybe_parens_func($sub_name, $self->combine2str(', ', \@body), $cx, 5);
	    return B::DeparseTree::Node->new($op, $self, \@texts,
					     '', $type,
					     {other_ops => $other_ops});
	} else {
	    $type = 'call';
	    @texts = dedup_parens_func($self, $subname_info, \@body);
	    return B::DeparseTree::Node->new($op, $self, \@texts,
					     '', $type,
					     {other_ops => $other_ops});
	}
    }
    return $self->info_from_template($type, $op,
				     '%C', [[0, $#texts, '']], \@texts,
				     {other_ops => $other_ops});
}

sub pp_gv
{
    my($self, $op, $cx) = @_;
    my $gv = $self->gv_or_padgv($op);
    my $name = $self->gv_name($gv);
    return $self->info_from_string("global variable $name", $op, $name);
}

sub pp_once
{
    my ($self, $op, $cx) = @_;
    my $cond = $op->first;
    my $true = $cond->sibling;

    return $self->deparse($true, $cx);
}

sub pp_print { indirop(@_, "print") }
sub pp_prtf { indirop(@_, "printf") }

sub pp_rand { maybe_targmy(@_, \&unop, "rand") }

# 'x' is weird when the left arg is a list
sub pp_repeat {
    my $self = shift;
    my($op, $cx) = @_;
    my $left = $op->first;
    my $right = $op->last;
    my $eq = "";
    my $prec = 19;
    my $other_ops = undef;
    if ($op->flags & OPf_STACKED) {
	$eq = "=";
	$prec = 7;
    }
    my @exprs = ();
    my ($left_info, @body);
    if (null($right)) {
	# list repeat; count is inside left-side ex-list
	$other_ops = [$left->first];
	my $kid = $left->first->sibling; # skip pushmark
	for (my $i=0; !null($kid->sibling); $kid = $kid->sibling) {
	    my $expr = $self->deparse($kid, 6, $op);
	    push @exprs, $expr;
	}
	$right = $kid;
	@body = @exprs;
	$left_info = info_from_list($op, $self,
				    ["(", @exprs, ")"], '', 'repeat_left', {});
    } else {
	$left_info = $self->deparse_binop_left($op, $left, $prec);
    }
    my $right_info  = $self->deparse_binop_right($op, $right, $prec);
    my $texts = [$left_info, "x$eq", $right_info];
    my $info = info_from_list($op, $self, $texts, ' ', 'repeat',
			      {maybe_parens => [$self, $cx, $prec]});
    $info->{other_ops} = $other_ops if $other_ops;
    return $info
}

sub pp_say  { indirop(@_, "say") }
sub pp_srand { unop(@_, "srand") }
sub pp_setstate { pp_nextstate(@_) }
sub pp_sort { indirop(@_, "sort") }

sub pp_or  { logop(@_, "or",  2, "||", 10, "unless") }
sub pp_dor { logop(@_, "//", 10) }

sub pp_mapwhile { mapop(@_, "map") }
sub pp_grepwhile { mapop(@_, "grep") }

sub pp_complement { maybe_targmy(@_, \&pfixop, "~", 21) }
sub pp_getppid { maybe_targmy(@_, \&baseop, "getppid") }
sub pp_postdec { maybe_targmy(@_, \&pfixop, "--", 23, POSTFIX) }
sub pp_postinc { maybe_targmy(@_, \&pfixop, "++", 23, POSTFIX) }
sub pp_time { maybe_targmy(@_, \&baseop, "time") }

sub pp_preinc { pfixop(@_, "++", 23) }
sub pp_predec { pfixop(@_, "--", 23) }
sub pp_i_preinc { pfixop(@_, "++", 23) }
sub pp_i_predec { pfixop(@_, "--", 23) }

sub pp_substr {
    my ($self,$op,$cx) = @_;
    if ($op->private & B::Deparse::OPpSUBSTR_REPL_FIRST) {
	my $left = listop($self, $op, 7, "substr", $op->first->sibling->sibling);
	my $right = $self->deparse($op->first->sibling, 7, $op);
	return info_from_list($op, $self,[$left, '=', $right], ' ',
			      'substr_repl_first', {});
    }
    return maybe_local(@_, listop(@_, "substr"))
}
# FIXME:
# Different between 5.20 and 5.22. We've used 5.22 though.
# Go over and make sure this is okay.
sub pp_stub {
    my ($self, $op) = @_;
    $self->info_from_string('stub ()', $op, '()')
};

sub pp_symlink { maybe_targmy(@_, \&listop, "symlink") }

sub pp_undef { unop(@_, "undef") }

sub pp_unstack {
    my ($self, $op) = @_;
    # see also leaveloop
    return info_from_text($op, $self, '', 'unstack', {});
}

sub pp_wait { maybe_targmy(@_, \&baseop, "wait") }
sub pp_wantarray { baseop(@_, "wantarray") }

# xor is syntactically a logop, but it's really a binop (contrary to
# old versions of opcode.pl). Syntax is what matters here.
sub pp_xor { logop(@_, "xor", 2, "",   0,  "") }

1;
