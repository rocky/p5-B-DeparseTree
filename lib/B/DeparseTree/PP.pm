# Common PP opcodes. Specifc Perl versions can override these.

# Copyright (c) 2015, 2018 Rocky Bernstein
#
use strict;
use warnings ();

use rlib '../..';

package B::DeparseTree::PP;

use B::DeparseTree::Common;
use B::DeparseTree::Node;

use B qw(OPpEXISTS_SUB OPf_SPECIAL);

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.0.0';

@ISA = qw(Exporter);
@EXPORT = qw(

    pp_cond_expr
    pp_egrent pp_ehostent pp_enetent
    pp_eprotoent pp_epwent pp_eservent
    pp_fork pp_getlogin pp_ggrent
    pp_ghostent pp_gnetent pp_gprotoent
    pp_gpwent pp_grepstart pp_gservent
    pp_mapstart
    pp_sgrent pp_spwent pp_tms pp_wantarray

    pp_leave pp_lineseq
    pp_once
    pp_scope

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
    my $self = shift;
    my($op, $cx) = @_;
    $self->{'curcop'} = $op;
    my @texts;
    push @texts, $self->cop_subs($op);
    if (@texts) {
	# Special marker to swallow up the semicolon
	push @texts, "\cK";
    }

    my $stash = $op->stashpv;
    if ($stash ne $self->{'curstash'}) {
	push @texts, $self->keyword("package") . " $stash;\n";
	$self->{'curstash'} = $stash;
    }

    if (OPpCONST_ARYBASE && $self->{'arybase'} != $op->arybase) {
	push @texts, '$[ = '. $op->arybase .";\n";
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
    	push @texts,
    	    $self->declare_warnings($self->{'warnings'}, $warning_bits);
    	$self->{'warnings'} = $warning_bits;
    }

    my $hints = $] < 5.008009 ? $op->private : $op->hints;
    my $old_hints = $self->{'hints'};
    if ($self->{'hints'} != $hints) {
	push @texts, $self->declare_hints($self->{'hints'}, $hints);
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
		    $self->keyword("no") . " feature ':all';\n",
		    $self->keyword("use") . " feature ':$bundle';\n";
	    }
	}
    }

    if ($] > 5.009) {
	push @texts, $self->declare_hinthash(
	    $self->{'hinthash'}, $newhh,
	    $self->{indent_size}, $self->{hints},
	);
	$self->{'hinthash'} = $newhh;
    }


    # This should go after of any branches that add statements, to
    # increase the chances that it refers to the same line it did in
    # the original program.
    if ($self->{'linenums'} && $cx != .5) { # $cx == .5 means in a format
	my $line = sprintf("\n# line %s '%s'", $op->line, $op->file);
	$line .= sprintf(" 0x%x", $$op) if $self->{'opaddr'};
	push @texts, $line . "\cK\n";
    }

    push @texts, $op->label . ": " if $op->label;

    my $info = B::DeparseTree::Node->new($op, $self->{deparse},
					 \@texts, '', 'pp_nextstate', {});
    return $info;
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
    unless ($cx < 1 and (is_scope($true) and $true->name ne "null") and
	    (is_scope($false) || is_ifelse_cont($false))
	    and $self->{'expand'} < 7) {
	my $cond_info = $self->deparse($cond, 8, $op);
	my $true_info = $self->deparse($true, 6, $op);
	my $false_info = $self->deparse($false, 8, $op);
	my @texts = ($cond_info->{text}, '?', $true_info->{text}, ':', $false_info->{text});
	return info_from_list($op, $self, \@texts, ' ', 'pp_cond_expr1',
				  {maybe_parens => [$self, $cx, 8]});
    }

    my $cond_info = $self->deparse($cond, 1, $op);
    my $true_info = $self->deparse($true, 0, $op);
    my @head = ('if ', '(', $cond_info->{text}, ') ', "{\n\t", $true_info->{text}, "\n\b}");
    my @elsifs;

    while (!null($false) and is_ifelse_cont($false)) {
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
	push @elsifs, ('', "elsif ($newcond_info->{text})",
		       "{\n\t",
		       $newtrue_info->{text},
		       "\n\b}");
    }
    my $false_info;
    if (!null($false)) {
	$false_info = $self->deparse($false, 0, $op);
	$false_info->{text} = $cuddle . "else {\n\t" . $false_info->{text} . "\n\b}\cK";
    } else {
	$false_info->{text} = "\cK";
    }
    my @texts = (@head, @elsifs, $false_info->{text});
    my $text = join('', @head) . join($cuddle, @elsifs) . $false_info->{text};
    return info_from_list($op, $self, \@texts, ' ', 'pp_cond_expr', {});
}

sub pp_dbstate { pp_nextstate(@_) }

sub pp_once
{
    my ($self, $op, $cx) = @_;
    my $cond = $op->first;
    my $true = $cond->sibling;

    return $self->deparse($true, $cx);
}

sub pp_print { indirop(@_, "print") }
sub pp_prtf { indirop(@_, "printf") }
sub pp_say  { indirop(@_, "say") }

sub pp_setstate { pp_nextstate(@_) }

sub pp_sort { indirop(@_, "sort") }


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
sub pp_stub {
    my ($self, $op) = @_;
    info_from_list($op, $self, ["(", ")"], '', 'stub', {})
};

sub pp_exists
{
    my($self, $op, $cx) = @_;
    my ($info, $type);
    my $name = $self->keyword("exists");
    if ($op->private & OPpEXISTS_SUB) {
	# Checking for the existence of a subroutine
	$info = $self->pp_rv2cv($op->first, 16);
	$type = 'exists_sub';
    } elsif ($op->flags & OPf_SPECIAL) {
	# Array element, not hash helement
	$info = $self->pp_aelem($op->first, 16);
	$type = 'info_array';
    } else {
	$info = $self->pp_helem($op->first, 16);
	$type = 'info_hash';
    }
    my @texts = $self->maybe_parens_func($name, $info->{text}, $cx, 16);
    return info_from_list($op, $self, \@texts, '', $type, {body=>[$info]});
}
