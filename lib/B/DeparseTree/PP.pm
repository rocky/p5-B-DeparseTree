# Common PP opcodes. Specifc Perl versions can override these.

# Copyright (c) 2015 Rocky Bernstein
#
use strict;
use warnings ();

use rlib '../..';

package B::DeparseTree::PP;

use B::DeparseTree::Common;

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.0.0';

@ISA = qw(Exporter);
@EXPORT = qw(
    pp_leave pp_lineseq pp_scope
    pp_dbstate pp_nextstate pp_setstate
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

sub _features_from_bundle {
    my ($hints, $hh) = @_;
    foreach (@{$feature::feature_bundle{@feature::hint_bundles[$hints >> $feature::hint_shift]}}) {
	$hh->{$feature::feature{$_}} = 1;
    }
    return $hh;
}

sub pp_scope { scopeop(0, @_); }
sub pp_lineseq { scopeop(0, @_); }
sub pp_leave { scopeop(1, @_); }

# Notice how subs and formats are inserted between statements here;
# also $[ assignments and pragmas.
sub pp_nextstate {
    my($self, $op, $cx) = @_;
    $self->{'curcop'} = $op;
    my @text = map($_->{text}, $self->cop_subs($op));
    my $stash = $op->stashpv;
    if ($stash ne $self->{'curstash'}) {
	push @text, "package $stash;\n";
	$self->{'curstash'} = $stash;
    }

    if (OPpCONST_ARYBASE && $self->{'arybase'} != $op->arybase) {
	push @text, '$[ = '. $op->arybase .";\n";
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
	push @text, declare_warnings($self->{'warnings'}, $warning_bits);
	$self->{'warnings'} = $warning_bits;
    }

    my $hints = $op->hints;
    my $old_hints = $self->{'hints'};
    if ($self->{'hints'} != $hints) {
	push @text, declare_hints($self->{'hints'}, $hints);
	$self->{'hints'} = $hints;
    }

    my $newhh = $op->hints_hash->HASH;

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
	    push @text, "no feature;\n",
		"use feature ':$bundle';\n";
	}
    }

    push @text, declare_hinthash(
	$self->{'hinthash'}, $newhh,
	$self->{indent_size}, $self->{hints},
	);
    $self->{'hinthash'} = $newhh;

    # This should go after of any branches that add statements, to
    # increase the chances that it refers to the same line it did in
    # the original program.
    if ($self->{'linenums'}) {
	my $line = sprintf("\n# line %s '%s'", $op->line, $op->file);
	$line .= sprintf(" 0x%x", $$op) if $self->{'opaddr'};
	push @text, $line . "\cK\n";
    }

    push @text, $op->label . ": " if $op->label;

    return {
	op => $op,
	texts => \@text,
	text => join("", @text)
    }
}

sub pp_dbstate { pp_nextstate(@_) }
sub pp_setstate { pp_nextstate(@_) }

1;
