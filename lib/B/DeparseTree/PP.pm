# Common PP opcodes. Specifc Perl versions can override these.

# Copyright (c) 2015 Rocky Bernstein
#
use strict;
use warnings ();

use rlib '../..';

package B::DeparseTree::PP;

use B::DeparseTree::Common;

use B qw(OPpEXISTS_SUB OPf_SPECIAL);

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.0.0';

@ISA = qw(Exporter);
@EXPORT = qw(

    pp_egrent pp_ehostent pp_enetent
    pp_eprotoent pp_epwent pp_eservent
    pp_fork pp_getlogin pp_ggrent
    pp_ghostent pp_gnetent pp_gprotoent
    pp_gpwent pp_grepstart pp_gservent
    pp_mapstart
    pp_sgrent pp_spwent pp_tms pp_wantarray

    pp_leave pp_lineseq pp_scope

    pp_dbstate pp_nextstate pp_setstate

    pp_and pp_or pp_dor pp_xor

    pp_mapwhile pp_grepwhile

    pp_complement
    pp_getppid
    pp_postdec
    pp_postinc
    pp_time
    pp_wait

    pp_print pp_prtf pp_say pp_sort
    pp_preinc pp_predec pp_i_preinc pp_i_predec

    pp_stub pp_exists
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

sub pp_egrent { baseop(@_, "endgrent") }
sub pp_ehostent { baseop(@_, "endhostent") }
sub pp_enetent { baseop(@_, "endnetent") }
sub pp_eprotoent { baseop(@_, "endprotoent") }
sub pp_epwent { baseop(@_, "endpwent") }
sub pp_eservent { baseop(@_, "endservent") }
sub pp_fork { baseop(@_, "fork") }
sub pp_getlogin { baseop(@_, "getlogin") }
sub pp_ggrent { baseop(@_, "getgrent") }
sub pp_ghostent { baseop(@_, "gethostent") }
sub pp_gnetent { baseop(@_, "getnetent") }
sub pp_gprotoent { baseop(@_, "getprotoent") }
sub pp_gpwent { baseop(@_, "getpwent") }
sub pp_grepstart { baseop(@_, "grep") }
sub pp_gservent { baseop(@_, "getservent") }
sub pp_mapstart { baseop(@_, "map") }
sub pp_sgrent { baseop(@_, "setgrent") }
sub pp_spwent { baseop(@_, "setpwent") }
sub pp_tms { baseop(@_, "times") }
sub pp_wantarray { baseop(@_, "wantarray") }

sub pp_leave { scopeop(1, @_); }
sub pp_lineseq { scopeop(0, @_); }
sub pp_scope { scopeop(0, @_); }

# Notice how subs and formats are inserted between statements here;
# also $[ assignments and pragmas.
sub pp_nextstate {
    my($self, $op, $cx) = @_;
    $self->{'curcop'} = $op;

    my @text;

    my @subs = $self->cop_subs($op);
    if (@subs) {
	# Special marker to swallow up the semicolon
	push @subs, "\cK";
    }
    push @text, @subs;

    push @text, $self->pragmata($op);


    # This should go after of any branches that add statements, to
    # increase the chances that it refers to the same line it did in
    # the original program.
    if ($self->{'linenums'} && $cx != .5) { # $cx == .5 means in a format
	push @text, "\f#line " . $op->line .
	  ' "' . $op->file, qq'"\n';
    }

    push @text, $op->label . ": " if $op->label;

    return {
	op => $op,
	texts => \@text,
	text => join("", @text)
    }
}

sub pp_print { indirop(@_, "print") }
sub pp_prtf { indirop(@_, "printf") }
sub pp_say  { indirop(@_, "say") }
sub pp_sort { indirop(@_, "sort") }

sub pp_dbstate { pp_nextstate(@_) }
sub pp_setstate { pp_nextstate(@_) }

sub pp_and { logop(@_, "and", 3, "&&", 11, "if") }
sub pp_or  { logop(@_, "or",  2, "||", 10, "unless") }
sub pp_dor { logop(@_, "//", 10) }

# xor is syntactically a logop, but it's really a binop (contrary to
# old versions of opcode.pl). Syntax is what matters here.
sub pp_xor { logop(@_, "xor", 2, "",   0,  "") }

sub pp_mapwhile { mapop(@_, "map") }
sub pp_grepwhile { mapop(@_, "grep") }

sub pp_complement { maybe_targmy(@_, \&pfixop, "~", 21) }
sub pp_getppid { maybe_targmy(@_, \&baseop, "getppid") }
sub pp_postdec { maybe_targmy(@_, \&pfixop, "--", 23, POSTFIX) }
sub pp_postinc { maybe_targmy(@_, \&pfixop, "++", 23, POSTFIX) }
sub pp_time { maybe_targmy(@_, \&baseop, "time") }
sub pp_wait { maybe_targmy(@_, \&baseop, "wait") }

sub pp_preinc { pfixop(@_, "++", 23) }
sub pp_predec { pfixop(@_, "--", 23) }
sub pp_i_preinc { pfixop(@_, "++", 23) }
sub pp_i_predec { pfixop(@_, "--", 23) }

# FIXME:
# Different between 5.20 and 5.20. We've used 5.22 tough
# Go over and make sure this is okay.
sub pp_stub {info_from_list(["(", ")"], '', 'stub', {})};

sub pp_exists
{
    my($self, $op, $cx) = @_;
    my ($info, $type);
    my $name = $self->keyword("exists");
    if ($op->private & OPpEXISTS_SUB) {
	# Checking for the existence of a subroutine
	$info = $self->pp_rv2cv($op->first, 16);
	$type = 'exists_sub';
    }
    if ($op->flags & OPf_SPECIAL) {
	# Array element
	$info = $self->pp_aelem($op->first, 16);
	$type = 'info_array';
    } else {
	$info = $self->pp_helem($op->first, 16);
	$type = 'info_hash';
    }
    my @texts = $self->maybe_parens_func($name, $info->{text}, $cx, 16);
    return info_from_list(\@texts, '', $type, {body=>[$info]});
}
