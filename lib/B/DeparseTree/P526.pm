# B::DeparseTree::P526.pm
# Copyright (c) 1998-2000, 2002, 2003, 2004, 2005, 2006 Stephen McCamant.
# Copyright (c) 2015, 2017, 2018 Rocky Bernstein
# All rights reserved.
# This module is free software; you can redistribute and/or modify
# it under the same terms as Perl itself.

# This is based on the module B::Deparse (for perl 5.22) by Stephen McCamant.
# It has been extended save tree structure, and is addressible
# by opcode address.

# B::Parse in turn is based on the module of the same name by Malcolm Beattie,
# but essentially none of his code remains.

use v5.26;

use rlib '../..';

package B::DeparseTree::P526;
use Carp;

use B qw(class main_root main_start main_cv svref_2object opnumber perlstring
	 OPf_KIDS OPf_REF OPf_STACKED OPf_SPECIAL OPf_MOD OPf_PARENS
	 OPf_WANT OPf_WANT_VOID OPf_WANT_SCALAR OPf_WANT_LIST
	 OPpEXISTS_SUB OPpSORT_NUMERIC OPpSORT_INTEGER
	 OPpLVAL_INTRO OPpOUR_INTRO OPpENTERSUB_AMPER OPpSLICE OPpCONST_BARE
	 OPpSORT_REVERSE OPpMULTIDEREF_EXISTS OPpMULTIDEREF_DELETE
	 OPpTRANS_SQUASH OPpTRANS_DELETE OPpTRANS_COMPLEMENT OPpTARGET_MY
	 PADNAMEt_OUTER
	 PMf_KEEP PMf_GLOBAL PMf_CONTINUE PMf_EVAL PMf_ONCE
	 PMf_MULTILINE PMf_SINGLELINE PMf_FOLD PMf_EXTENDED PMf_EXTENDED_MORE
	 SVf_ROK SVpad_OUR SVf_FAKE SVs_RMG SVs_SMG
	 SVpad_TYPED
         CVf_METHOD
         MDEREF_ACTION_MASK
         MDEREF_AV_gvav_aelem
         MDEREF_AV_gvsv_vivify_rv2av_aelem
         MDEREF_AV_padav_aelem
         MDEREF_AV_padsv_vivify_rv2av_aelem
         MDEREF_AV_pop_rv2av_aelem
         MDEREF_AV_vivify_rv2av_aelem
         MDEREF_FLAG_last
         MDEREF_HV_gvhv_helem
         MDEREF_HV_gvsv_vivify_rv2hv_helem
         MDEREF_HV_padhv_helem
         MDEREF_HV_padsv_vivify_rv2hv_helem
         MDEREF_HV_pop_rv2hv_helem
         MDEREF_HV_vivify_rv2hv_helem
         MDEREF_INDEX_MASK
         MDEREF_INDEX_const
         MDEREF_INDEX_gvsv
         MDEREF_INDEX_none
         MDEREF_INDEX_padsv
         MDEREF_MASK
         MDEREF_SHIFT
         MDEREF_reload
         OPpPADRANGE_COUNTSHIFT
         OPpSPLIT_ASSIGN OPpSPLIT_LEX
    );

use B::DeparseTree::PPfns;
use B::DeparseTree::Common;
use B::DeparseTree::SyntaxTree;
use B::DeparseTree::PP;
use B::Deparse;

# Copy unchanged functions from B::Deparse
*begin_is_use = *B::Deparse::begin_is_use;
*const_sv = *B::Deparse::const_sv;
*escape_re = *B::Deparse::escape_re;
*find_scope_st = *B::Deparse::find_scope_st;
*gv_name = *B::Deparse::gv_name;
*keyword = *B::Deparse::keyword;
*meth_pad_subs = *B::Deparse::pad_subs;
*meth_rclass_sv = *B::Deparse::meth_rclass_sv;
*meth_sv = *B::Deparse::meth_sv;
*padany = *B::Deparse::padany;
*padname = *B::Deparse::padname;
*padname_sv = *B::Deparse::padname_sv;
*padval = *B::Deparse::padval;
*re_flags = *B::Deparse::re_flags;
*stash_variable = *B::Deparse::stash_variable;
*stash_variable_name = *B::Deparse::stash_variable_name;
*tr_chr = *B::Deparse::tr_chr;

use strict;
use vars qw/$AUTOLOAD/;
use warnings ();
require feature;

our(@EXPORT, @ISA);
our $VERSION = '3.0.0';

@ISA = qw(Exporter B::DeparseTree::Common);
@EXPORT = qw(is_pp_null_list);

BEGIN {
    # List version-specific constants here.
    # Easiest way to keep this code portable between version looks to
    # be to fake up a dummy constant that will never actually be true.
    foreach (qw(OPpSORT_INPLACE OPpSORT_DESCEND OPpITER_REVERSED OPpCONST_NOVER
		OPpPAD_STATE PMf_SKIPWHITE RXf_SKIPWHITE
		PMf_CHARSET PMf_KEEPCOPY PMf_NOCAPTURE CVf_ANONCONST
		CVf_LOCKED OPpREVERSE_INPLACE OPpSUBSTR_REPL_FIRST
		PMf_NONDESTRUCT OPpCONST_ARYBASE OPpEVAL_BYTES
		OPpLVREF_TYPE OPpLVREF_SV OPpLVREF_AV OPpLVREF_HV
		OPpLVREF_CV OPpLVREF_ELEM SVpad_STATE)) {
	eval { B->import($_) };
	no strict 'refs';
	*{$_} = sub () {0} unless *{$_}{CODE};
    }
}

BEGIN { for (qw[ const stringify rv2sv list glob pushmark null aelem
		 nextstate dbstate rv2av rv2hv helem custom ]) {
    eval "sub OP_\U$_ () { " . opnumber($_) . "}"
}}

sub deparse_format($$$)
{
    my ($self, $form, $parent) = @_;
    my @texts;
    local($self->{'curcv'}) = $form;
    local($self->{'curcvlex'});
    local($self->{'in_format'}) = 1;
    local(@$self{qw'curstash warnings hints hinthash'})
		= @$self{qw'curstash warnings hints hinthash'};
    my $op = $form->ROOT;
    local $B::overlay = {};
    $self->pessimise($op, $form->START);
    my $info = {
	op  => $op,
	parent => $parent,
	cop => $self->{'curcop'}
    };
    $self->{optree}{$$op} = $info;

    if ($op->first->name eq 'stub' || $op->first->name eq 'nextstate') {
	my $info->{text} = "\f.";
	return $info;
    }

    $op->{other_ops} = [$op->first];
    $op = $op->first->first; # skip leavewrite, lineseq
    my $kid;
    while (not null $op) {
	push @{$op->{other_ops}}, $op;
	$op = $op->sibling; # skip nextstate
	my @body;
	push @{$op->{other_ops}}, $op->first;
	$kid = $op->first->sibling; # skip a pushmark
	push @texts, "\f".$self->const_sv($kid)->PV;
	push @{$op->{other_ops}}, $kid;
	$kid = $kid->sibling;
	for (; not null $kid; $kid = $kid->sibling) {
	    push @body, $self->deparse($kid, -1, $op);
	    $body[-1] =~ s/;\z//;
	}
	push @texts, "\f".$self->combine2str("\n", \@body) if @body;
	$op = $op->sibling;
    }

    $info->{text} = $self->combine2str(\@texts) . "\f.";
    $info->{texts} = \@texts;
    return $info;
}

sub ambient_pragmas {
    my $self = shift;
    my ($arybase, $hint_bits, $warning_bits, $hinthash) = (0, 0);

    while (@_ > 1) {
	my $name = shift();
	my $val  = shift();

	if ($name eq 'strict') {
	    require strict;

	    if ($val eq 'none') {
		$hint_bits &= $strict_bits{$_} for qw/refs subs vars/;
		next();
	    }

	    my @names;
	    if ($val eq "all") {
		@names = qw/refs subs vars/;
	    }
	    elsif (ref $val) {
		@names = @$val;
	    }
	    else {
		@names = split' ', $val;
	    }
	    $hint_bits |= $strict_bits{$_} for @names;
	}

	elsif ($name eq '$[') {
	    if (OPpCONST_ARYBASE) {
		$arybase = $val;
	    } else {
		croak "\$[ can't be non-zero on this perl" unless $val == 0;
	    }
	}

	elsif ($name eq 'integer'
	    || $name eq 'bytes'
	    || $name eq 'utf8') {
	    require "$name.pm";
	    if ($val) {
		$hint_bits |= ${$::{"${name}::"}{"hint_bits"}};
	    }
	    else {
		$hint_bits &= ~${$::{"${name}::"}{"hint_bits"}};
	    }
	}

	elsif ($name eq 're') {
	    require re;
	    if ($val eq 'none') {
		$hint_bits &= ~re::bits(qw/taint eval/);
		next();
	    }

	    my @names;
	    if ($val eq 'all') {
		@names = qw/taint eval/;
	    }
	    elsif (ref $val) {
		@names = @$val;
	    }
	    else {
		@names = split' ',$val;
	    }
	    $hint_bits |= re::bits(@names);
	}

	elsif ($name eq 'warnings') {
	    if ($val eq 'none') {
		$warning_bits = $warnings::NONE;
		next();
	    }

	    my @names;
	    if (ref $val) {
		@names = @$val;
	    }
	    else {
		@names = split/\s+/, $val;
	    }

	    $warning_bits = $warnings::NONE if !defined ($warning_bits);
	    $warning_bits |= warnings::bits(@names);
	}

	elsif ($name eq 'warning_bits') {
	    $warning_bits = $val;
	}

	elsif ($name eq 'hint_bits') {
	    $hint_bits = $val;
	}

	elsif ($name eq '%^H') {
	    $hinthash = $val;
	}

	else {
	    croak "Unknown pragma type: $name";
	}
    }
    if (@_) {
	croak "The ambient_pragmas method expects an even number of args";
    }

    $self->{'ambient_arybase'} = $arybase;
    $self->{'ambient_warnings'} = $warning_bits;
    $self->{'ambient_hints'} = $hint_bits;
    $self->{'ambient_hinthash'} = $hinthash;
}

# The following OPs don't have functions:

# pp_padany -- does not exist after parsing

sub AUTOLOAD {
    if ($AUTOLOAD =~ s/^.*::pp_//) {
	warn "unexpected OP_".
	  ($_[1]->type == OP_CUSTOM ? "CUSTOM ($AUTOLOAD)" : uc $AUTOLOAD);
	return "XXX";
    } else {
	Carp::confess "Undefined subroutine $AUTOLOAD called";
    }
}

sub DESTROY {}	#	Do not AUTOLOAD

# The BEGIN {} is used here because otherwise this code isn't executed
# when you run B::Deparse on itself.
my %globalnames;
BEGIN { map($globalnames{$_}++, "SIG", "STDIN", "STDOUT", "STDERR", "INC",
	    "ENV", "ARGV", "ARGVOUT", "_"); }

sub populate_curcvlex {
    my $self = shift;
    for (my $cv = $self->{'curcv'}; class($cv) eq "CV"; $cv = $cv->OUTSIDE) {
	my $padlist = $cv->PADLIST;
	# an undef CV still in lexical chain
	next if class($padlist) eq "SPECIAL";
	my @padlist = $padlist->ARRAY;
	my @ns = $padlist[0]->ARRAY;

	for (my $i=0; $i<@ns; ++$i) {
	    next if class($ns[$i]) eq "SPECIAL";
	    if (class($ns[$i]) eq "PV") {
		# Probably that pesky lexical @_
		next;
	    }
            my $name = $ns[$i]->PVX;
	    next unless defined $name;
	    my ($seq_st, $seq_en) =
		($ns[$i]->FLAGS & SVf_FAKE)
		    ? (0, 999999)
		    : ($ns[$i]->COP_SEQ_RANGE_LOW, $ns[$i]->COP_SEQ_RANGE_HIGH);

	    push @{$self->{'curcvlex'}{
			($ns[$i]->FLAGS & SVpad_OUR ? 'o' : 'm') . $name
		  }}, [$seq_st, $seq_en, $ns[$i]];
	}
    }
}

# Returns a list of subs which should be inserted before the COP
sub cop_subs {
    my ($self, $op, $out_seq) = @_;
    my $seq = $op->cop_seq;
    # If we have nephews, then our sequence number indicates
    # the cop_seq of the end of some sort of scope.
    if (class($op->sibling) ne "NULL" && $op->sibling->flags & OPf_KIDS
	and my $nseq = $self->find_scope_st($op->sibling) ) {
	$seq = $nseq;
    }
    $seq = $out_seq if defined($out_seq) && $out_seq < $seq;
    return $self->seq_subs($seq);
}

sub is_pp_null_list($$) {
    my ($self, $op) = @_;
    return $op->name eq 'pushmark' or
	($op->name eq 'null'
	 && $op->targ == B::Deparse::OP_PUSHMARK
	 && B::Deparse::_op_is_or_was($op, B::Deparse::OP_LIST));
}

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

# keywords that are strong and also have a prototype
#
my %strong_proto_keywords = map { $_ => 1 } qw(
    pos
    prototype
    scalar
    study
    undef
);

sub feature_enabled {
	my($self,$name) = @_;
	my $hh;
	my $hints = $self->{hints} & $feature::hint_mask;
	if ($hints && $hints != $feature::hint_mask) {
	    $hh = _features_from_bundle($hints);
	}
	elsif ($hints) { $hh = $self->{'hinthash'} }
	return $hh && $hh->{"feature_$feature_keywords{$name}"}
}

# Note: maybe_local things can't be moved to PP yet.
sub pp_pos { maybe_local(@_, unop(@_, "pos")) }

sub pp_each { unop(@_, "each") }
sub pp_values { unop(@_, "values") }
{ no strict 'refs'; *{"pp_r$_"} = *{"pp_$_"} for qw< keys each values >; }

# FIXME:
# Different in 5.20. Go over differences to see if okay in 5.20.
sub pp_chdir {
    my ($self, $op, $cx) = @_;
    if (($op->flags & (OPf_SPECIAL|OPf_KIDS)) == (OPf_SPECIAL|OPf_KIDS)) {
	my $kw = $self->keyword("chdir");
	my $kid = $self->const_sv($op->first)->PV;
	my $code = $kw
		 . ($cx >= 16 || $self->{'parens'} ? "($kid)" : " $kid");
	maybe_targmy(@_, sub { $_[3] }, $code);
    } else {
	maybe_targmy(@_, \&unop, "chdir")
    }
}

sub pp_entereval {
    unop(
      @_,
      $_[1]->private & OPpEVAL_BYTES ? 'evalbytes' : "eval"
    )
}

sub pp_dofile
{
    my $code = unop(@_, "do", 1); # llafr does not apply
    if ($code =~ s/^((?:CORE::)?do) \{/$1({/) { $code .= ')' }
    $code;
}

sub pp_leavegiven { givwhen(@_, $_[0]->keyword("given")); }
sub pp_leavewhen  { givwhen(@_, $_[0]->keyword("when")); }

sub pp_scalar
{
    my($self, $op, $cx) = @_;
    my $kid = $op->first;
    if (not null $kid->sibling) {
	# XXX Was a here-doc
	return $self->dquote($op);
    }
    $self->unop($op, $cx, "scalar");
}

sub pp_readline {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first;
    $kid = $kid->first if $kid->name eq "rv2gv"; # <$fh>
    if (B::Deparse::is_scalar($kid)
	and $op->flags & OPf_SPECIAL
	and $self->deparse($kid, 1) eq 'ARGV') {
	my $body = [$self->deparse($kid, 1, $op)];
	return info_from_list($op, $self, ['<', $body->[0]{text}, '>'], '',
			      'readline_scalar', {body=>$body});
    }
    return $self->unop($op, $cx, "readline");
}

sub pp_rcatline {
    my $self = shift;
    my($op) = @_;
    return info_from_list($op, $self, ["<", $self->gv_name($self->gv_or_padgv($op)), ">"],
			  '', 'rcatline', {});
}

sub SWAP_CHILDREN () { 1 }
sub ASSIGN () { 2 } # has OP= variant
sub LIST_CONTEXT () { 4 } # Assignment is in list context

sub pp_add { maybe_targmy(@_, \&binop, "+", 18, ASSIGN) }
sub pp_multiply { maybe_targmy(@_, \&binop, "*", 19, ASSIGN) }
sub pp_subtract { maybe_targmy(@_, \&binop, "-",18,  ASSIGN) }
sub pp_divide { maybe_targmy(@_, \&binop, "/", 19, ASSIGN) }
sub pp_modulo { maybe_targmy(@_, \&binop, "%", 19, ASSIGN) }
sub pp_i_add { maybe_targmy(@_, \&binop, "+", 18, ASSIGN) }
sub pp_i_multiply { maybe_targmy(@_, \&binop, "*", 19, ASSIGN) }
sub pp_i_subtract { maybe_targmy(@_, \&binop, "-", 18, ASSIGN) }
sub pp_i_divide { maybe_targmy(@_, \&binop, "/", 19, ASSIGN) }
sub pp_i_modulo { maybe_targmy(@_, \&binop, "%", 19, ASSIGN) }
sub pp_pow { maybe_targmy(@_, \&binop, "**", 22, ASSIGN) }

sub pp_left_shift { maybe_targmy(@_, \&binop, "<<", 17, ASSIGN) }
sub pp_right_shift { maybe_targmy(@_, \&binop, ">>", 17, ASSIGN) }
sub pp_bit_and { maybe_targmy(@_, \&binop, "&", 13, ASSIGN) }
sub pp_bit_or { maybe_targmy(@_, \&binop, "|", 12, ASSIGN) }
sub pp_bit_xor { maybe_targmy(@_, \&binop, "^", 12, ASSIGN) }
*pp_nbit_and = *pp_bit_and;
*pp_nbit_or  = *pp_bit_or;
*pp_nbit_xor = *pp_bit_xor;
sub pp_sbit_and { maybe_targmy(@_, \&binop, "&.", 13, ASSIGN) }
sub pp_sbit_or { maybe_targmy(@_, \&binop, "|.", 12, ASSIGN) }
sub pp_sbit_xor { maybe_targmy(@_, \&binop, "^.", 12, ASSIGN) }

sub pp_eq { binop(@_, "==", 14) }
sub pp_ne { binop(@_, "!=", 14) }
sub pp_lt { binop(@_, "<", 15) }
sub pp_gt { binop(@_, ">", 15) }
sub pp_ge { binop(@_, ">=", 15) }
sub pp_le { binop(@_, "<=", 15) }
sub pp_cmp { binop(@_, "<=>", 14) }
sub pp_i_eq { binop(@_, "==", 14) }
sub pp_i_ne { binop(@_, "!=", 14) }
sub pp_i_lt { binop(@_, "<", 15) }
sub pp_i_gt { binop(@_, ">", 15) }
sub pp_i_ge { binop(@_, ">=", 15) }
sub pp_i_le { binop(@_, "<=", 15) }
sub pp_i_cmp { maybe_targmy(@_, \&binop, "<=>", 14) }

sub pp_seq { binop(@_, "eq", 14) }
sub pp_sne { binop(@_, "ne", 14) }
sub pp_slt { binop(@_, "lt", 15) }
sub pp_sgt { binop(@_, "gt", 15) }
sub pp_sge { binop(@_, "ge", 15) }
sub pp_sle { binop(@_, "le", 15) }
sub pp_scmp { binop(@_, "cmp", 14) }

sub pp_sassign { binop(@_, "=", 7, SWAP_CHILDREN) }

sub pp_smartmatch {
    my ($self, $op, $cx) = @_;
    if ($op->flags & OPf_SPECIAL) {
	return $self->deparse($op->last, $cx, $op);
    }
    else {
	binop(@_, "~~", 14);
    }
}

sub bin_info_join($$$$$$$) {
    my ($self, $op, $lhs, $rhs, $mid, $sep, $type) = @_;
    my $texts = [$lhs->{text}, $mid, $rhs->{text}];
    return info_from_list($op, $self, $texts, ' ', $type, {})
}

sub bin_info_join_maybe_parens($$$$$$$$$) {
    my ($self, $op, $lhs, $rhs, $mid, $sep, $cx, $prec, $type) = @_;
    my $info = bin_info_join($self, $op, $lhs, $rhs, $mid, $sep, $type);
    $info->{text} = $self->maybe_parens($info->{text}, $cx, $prec);
    return $info;
}

sub for_loop {
    my $self = shift;
    my($op, $cx, $parent) = @_;
    my $init = $self->deparse($op, 1, $parent);
    my $s = $op->sibling;
    my $ll = $s->name eq "unstack" ? $s->sibling : $s->first->sibling;
    return $self->loop_common($ll, $cx, $init);
}

sub pp_padsv {
    my $self = shift;
    my($op, $cx, $forbid_parens) = @_;
    return $self->maybe_my($op, $cx, $self->padname($op->targ),
			   $forbid_parens);
}

my @threadsv_names = B::threadsv_names;
sub pp_threadsv {
    my $self = shift;
    my($op, $cx) = @_;
    return $self->maybe_local_str($op, $cx, "\$" .  $threadsv_names[$op->targ]);
}

sub gv_or_padgv {
    my $self = shift;
    my $op = shift;
    if (class($op) eq "PADOP") {
	return $self->padval($op->padix);
    } else { # class($op) eq "SVOP"
	return $op->gv;
    }
}

# FIXME: adjust use of maybe_local_str
sub pp_gvsv
{
    my($self, $op, $cx) = @_;
    my $gv = $self->gv_or_padgv($op);
    return $self->maybe_local_str($op, $cx,
				  $self->stash_variable("\$",
							$self->gv_name($gv), $cx));
}

sub pp_aelemfast_lex
{
    my($self, $op, $cx) = @_;
    my $name = $self->padname($op->targ);
    $name =~ s/^@/\$/;
    return info_from_list($op, $self, [$name, "[", ($op->private + $self->{'arybase'}), "]"],
		      '', 'pp_aelemfast_lex', {});
}

sub pp_aelemfast
{
    my($self, $op, $cx) = @_;
    # optimised PADAV, pre 5.15
    return $self->pp_aelemfast_lex(@_) if ($op->flags & OPf_SPECIAL);

    my $gv = $self->gv_or_padgv($op);
    my($name,$quoted) = $self->stash_variable_name('@',$gv);
    $name = $quoted ? "$name->" : '$' . $name;
    my $i = $op->private;
    $i -= 256 if $i > 127;
    return info_from_list($op, $self, [$name, "[", ($op->private + $self->{'arybase'}), "]"],
		      '', 'pp_aelemfast', {});
}

sub pp_rv2sv { maybe_local_str(@_, rv2x(@_, "\$")) }
sub pp_rv2hv { maybe_local_str(@_, rv2x(@_, "%")) }
sub pp_rv2gv { maybe_local_str(@_, rv2x(@_, "*")) }

# skip rv2av
sub pp_av2arylen {
    my $self = shift;
    my($op, $cx) = @_;
    if ($op->first->name eq "padav") {
	return $self->maybe_local_str($op, $cx, '$#' . $self->padany($op->first));
    } else {
	return $self->maybe_local_str($op, $cx,
				      $self->rv2x($op->first, $cx, '$#'));
    }
}

# skip down to the old, ex-rv2cv
sub pp_rv2cv {
    my ($self, $op, $cx) = @_;
    my ($type, $str);
    if (!null($op->first) && $op->first->name eq 'null' &&
	$op->first->targ == OP_LIST)
    {
	$str = $self->rv2x($op->first->first->sibling, $cx, "&");
	$type = 'rv2cv_list';
    }
    else {
	$str = $self->rv2x($op, $cx, "");
	$type = 'rv2cv';
    }
    return info_from_text($op, $self, $str, $type, {});
}

sub list_const($$$) {
    my $self = shift;
    my($op, $cx, @list) = @_;
    my @a = map $self->const($_, 6), @list;
    my @texts = $self->map_texts(\@a);
    my $type = 'list_const';
    my $prec = 6;
    if (@texts == 0) {
	return info_from_list($op, $self, ['(', ')'], '', 'list_const_null', {});
    } elsif (@texts == 1) {
	return info_from_text($op, $self, $texts[0], 'list_const_one',
	    {body => \@a});
    } elsif ( @texts > 2 and !grep(!/^-?\d+$/, @texts)) {
	# collapse (-1,0,1,2) into (-1..2)
	my ($s, $e) = @texts[0,-1];
	my $i = $s;
	unless (grep $i++ != $_, @texts) {
	    @texts = ($s, '..', $e);
	    $type = 'list_const_range';
	    $prec = 9;
	}
    }
    return info_from_list($op, $self, \@texts,  '', $type,
	{maybe_parens => [$self, $cx, $prec]});
}

sub pp_rv2av {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first;
    if ($kid->name eq "const") { # constant list
	my $av = $self->const_sv($kid);
	return $self->list_const($kid, $cx, $av->ARRAY);
    } else {
	return $self->maybe_local_str($op, $cx,
				      $self->rv2x($op, $cx, "\@"));
    }
 }

sub elem_or_slice_array_name
{
    my $self = shift;
    my ($array, $left, $padname, $allow_arrow) = @_;

    if ($array->name eq $padname) {
	return $self->padany($array);
    } elsif (B::Deparse::is_scope($array)) { # ${expr}[0]
	return "{" . $self->deparse($array, 0) . "}";
    } elsif ($array->name eq "gv") {
	($array, my $quoted) =
	    $self->stash_variable_name(
		$left eq '[' ? '@' : '%', $self->gv_or_padgv($array)
	    );
	if (!$allow_arrow && $quoted) {
	    # This cannot happen.
	    die "Invalid variable name $array for slice";
	}
	return $quoted ? "$array->" : $array;
    } elsif (!$allow_arrow || B::Deparse::is_scalar $array) {
	# $x[0], $$x[0], ...
	return $self->deparse($array, 24)->{text};
    } else {
	return undef;
    }
}

sub elem_or_slice_single_index($$)
{
    my ($self, $idx, $parent) = @_;

    my $idx_info = $self->deparse($idx, 1, $parent);
    my $idx_str = $idx_info->{text};

    # Outer parens in an array index will confuse perl
    # if we're interpolating in a regular expression, i.e.
    # /$x$foo[(-1)]/ is *not* the same as /$x$foo[-1]/
    #
    # If $self->{parens}, then an initial '(' will
    # definitely be paired with a final ')'. If
    # !$self->{parens}, the misleading parens won't
    # have been added in the first place.
    #
    # [You might think that we could get "(...)...(...)"
    # where the initial and final parens do not match
    # each other. But we can't, because the above would
    # only happen if there's an infix binop between the
    # two pairs of parens, and *that* means that the whole
    # expression would be parenthesized as well.]
    #
    $idx_str =~ s/^\((.*)\)$/$1/ if $self->{'parens'};

    # Hash-element braces will autoquote a bareword inside themselves.
    # We need to make sure that C<$hash{warn()}> doesn't come out as
    # C<$hash{warn}>, which has a quite different meaning. Currently
    # B::Deparse will always quote strings, even if the string was a
    # bareword in the original (i.e. the OPpCONST_BARE flag is ignored
    # for constant strings.) So we can cheat slightly here - if we see
    # a bareword, we know that it is supposed to be a function call.
    #
    $idx_str =~ s/^([A-Za-z_]\w*)$/$1()/;

    return info_from_text($idx_info->{op}, $self, $idx_str,
			  'elem_or_slice_single_index',
			  {body => [$idx_info]});
}

sub elem
{
    my ($self, $op, $cx, $left, $right, $padname) = @_;
    my($array, $idx) = ($op->first, $op->first->sibling);

    my $idx_info = $self->elem_or_slice_single_index($idx, $op);
    my $opts = {body => [$idx_info]};

    unless ($array->name eq $padname) { # Maybe this has been fixed
	$opts->{other_ops} = [$array];
	$array = $array->first; # skip rv2av (or ex-rv2av in _53+)
    }
    my @texts = ();
    my $info;
    my $array_name=$self->elem_or_slice_array_name($array, $left, $padname, 1);
    if ($array_name) {
	if ($array_name !~ /->\z/) {
	    if ($array_name eq '#') {
		$array_name = '${#}';
	    }  else {
		$array_name = '$' . $array_name ;
	    }
	}
	push @texts, $array_name;
	push @texts, $left if $left;
	push @texts, $idx_info->{text}, $right;
	return info_from_list($op, $self, \@texts, '', 'elem', $opts)
    } else {
	# $x[20][3]{hi} or expr->[20]
	my $type;
	my $array_info = $self->deparse($array, 24, $op);
	push @{$info->{body}}, $array_info;
	@texts = ($array_info->{text});
	if (is_subscriptable($array)) {
	    push @texts, $left, $idx_info->{text}, $right;
	    $type = 'elem_no_arrow';
	} else {
	    push @texts, '->', $left, $idx_info->{text}, $right;
	    $type = 'elem_arrow';
	}
	return info_from_list($op, $self, \@texts, '', $type, $opts);
    }
    Carp::confess("unhandled condition in elem");
}

# a simplified version of elem_or_slice_array_name()
# for the use of pp_multideref

sub multideref_var_name($$$)
{
    my ($self, $gv, $is_hash) = @_;

    my ($name, $quoted) =
        $self->stash_variable_name( $is_hash  ? '%' : '@', $gv);
    return $quoted ? "$name->"
                   : $name eq '#'
                        ? '${#}'       # avoid ${#}[1] => $#[1]
                        : '$' . $name;
}

sub pp_multideref
{
    my($self, $op, $cx) = @_;
    my @texts = ();

    if ($op->private & OPpMULTIDEREF_EXISTS) {
        @texts = ($self->keyword("exists"), ' ');
    }
    elsif ($op->private & OPpMULTIDEREF_DELETE) {
        @texts = ($self->keyword("delete"), ' ')
    }
    elsif ($op->private & OPpLVAL_INTRO) {
        @texts = ($self->keyword("local"), ' ')
    }

    if ($op->first && ($op->first->flags & OPf_KIDS)) {
        # arbitrary initial expression, e.g. f(1,2,3)->[...]
	my $first = $self->deparse($op->first, 24, $op);
	push @texts, $first->{text};
    }

    my @items = $op->aux_list($self->{curcv});
    my $actions = shift @items;

    my $is_hash;
    my $derefs = 0;

    while (1) {
        if (($actions & MDEREF_ACTION_MASK) == MDEREF_reload) {
            $actions = shift @items;
            next;
        }

        $is_hash = (
	    ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_pop_rv2hv_helem
	    || ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_gvsv_vivify_rv2hv_helem
	    || ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_padsv_vivify_rv2hv_helem
	    || ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_vivify_rv2hv_helem
	    || ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_padhv_helem
	    || ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_gvhv_helem
	    );

        if (   ($actions & MDEREF_ACTION_MASK) == MDEREF_AV_padav_aelem
	       || ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_padhv_helem)
        {
            $derefs = 1;
            push @texts, '$' . substr($self->padname(shift @items), 1);
        }
        elsif (   ($actions & MDEREF_ACTION_MASK) == MDEREF_AV_gvav_aelem
		  || ($actions & MDEREF_ACTION_MASK) == MDEREF_HV_gvhv_helem)
        {
            $derefs = 1;
            push @texts, $self->multideref_var_name(shift @items, $is_hash);
        }
        else {
            if (   ($actions & MDEREF_ACTION_MASK) ==
		   MDEREF_AV_padsv_vivify_rv2av_aelem
		   || ($actions & MDEREF_ACTION_MASK) ==
		   MDEREF_HV_padsv_vivify_rv2hv_helem)
            {
                push @texts, $self->padname(shift @items);
            }
            elsif (   ($actions & MDEREF_ACTION_MASK) ==
		      MDEREF_AV_gvsv_vivify_rv2av_aelem
		      || ($actions & MDEREF_ACTION_MASK) ==
		      MDEREF_HV_gvsv_vivify_rv2hv_helem)
            {
                push @texts, $self->multideref_var_name(shift @items, $is_hash);
            }
            elsif (   ($actions & MDEREF_ACTION_MASK) ==
		      MDEREF_AV_pop_rv2av_aelem
		      || ($actions & MDEREF_ACTION_MASK) ==
		      MDEREF_HV_pop_rv2hv_helem)
            {
                if (   ($op->flags & OPf_KIDS)
		       && (   B::Deparse::_op_is_or_was($op->first, OP_RV2AV)
			      || B::Deparse::_op_is_or_was($op->first, OP_RV2HV))
		       && ($op->first->flags & OPf_KIDS)
		       && (   B::Deparse::_op_is_or_was($op->first->first, OP_AELEM)
			      || B::Deparse::_op_is_or_was($op->first->first, OP_HELEM))
                    )
                {
                    $derefs++;
                }
            }

            push(@texts, '->') if !$derefs++;
        }


        if (($actions & MDEREF_INDEX_MASK) == MDEREF_INDEX_none) {
            last;
        }

        push(@texts, $is_hash ? '{' : '[');

        if (($actions & MDEREF_INDEX_MASK) == MDEREF_INDEX_const) {
            my $key = shift @items;
            if ($is_hash) {
                push @texts, $self->const($key, $cx)->{text};
            }
            else {
                push @texts, $key;
            }
        }
        elsif (($actions & MDEREF_INDEX_MASK) == MDEREF_INDEX_padsv) {
            push @texts, $self->padname(shift @items);
	}
	elsif (($actions & MDEREF_INDEX_MASK) == MDEREF_INDEX_gvsv) {
	    push @texts,('$' .  ($self->stash_variable_name('$', shift @items))[0]);
	}

	push(@texts, $is_hash ? '}' : ']');

        if ($actions & MDEREF_FLAG_last) {
            last;
        }
        $actions >>= MDEREF_SHIFT;
    }

    return info_from_list($op, $self, \@texts, '', 'multideref', {});
}

sub pp_aelem { maybe_local(@_, elem(@_, "[", "]", "padav")) }
sub pp_helem { maybe_local(@_, elem(@_, "{", "}", "padhv")) }

sub pp_gelem
{
    my($self, $op, $cx) = @_;
    my($glob, $part) = ($op->first, $op->last);
    $glob = $glob->first; # skip rv2gv
    $glob = $glob->first if $glob->name eq "rv2gv"; # this one's a bug
    my $scope = is_scope($glob);
    $glob = $self->deparse($glob, 0);
    $part = $self->deparse($part, 1);
    return "*" . ($scope ? "{$glob}" : $glob) . "{$part}";
}

sub slice
{
    my ($self, $op, $cx, $left, $right, $regname, $padname) = @_;
    my $last;
    my(@elems, $kid, $array);
    if (class($op) eq "LISTOP") {
	$last = $op->last;
    } else { # ex-hslice inside delete()
	for ($kid = $op->first; !null $kid->sibling; $kid = $kid->sibling) {}
	$last = $kid;
    }
    $array = $last;
    $array = $array->first
	if $array->name eq $regname or $array->name eq "null";
    my $array_info = $self->elem_or_slice_array_name($array, $left, $padname, 0);
    $kid = $op->first->sibling; # skip pushmark

    if ($kid->name eq "list") {
	# FIXME:
	# skip list, pushmark
	$kid = $kid->first->sibling;
	for (; !null $kid; $kid = $kid->sibling) {
	    push @elems, $self->deparse($kid, 6, $op);
	}
    } else {
	@elems = ($self->elem_or_slice_single_index($kid, $op));
    }
    my $lead = '@';
    $lead = '%' if $op->name =~ /^kv/i;
    my ($fmt, $args_spec);
    my (@texts, $type);
    if ($array_info) {
	unshift @elems, $array_info;
	$fmt = "${lead}%c%$left%C$right";
	$args_spec = [0, [1, $#elems, ', ']];
	$type = "$lead<var>$left .. $right";
    } else {
	$fmt = "${lead}$left%C$right";
	$args_spec = [0, $#elems, ', '];
	$type = "${lead}$left .. $right";
    }
    return $self->info_from_template($type, $op, $fmt, $args_spec,
				     \@elems),
}

sub pp_aslice   { maybe_local(@_, slice(@_, "[", "]", "rv2av", "padav")) }
sub pp_kvaslice {                 slice(@_, "[", "]", "rv2av", "padav")  }
sub pp_hslice   { maybe_local(@_, slice(@_, "{", "}", "rv2hv", "padhv")) }
sub pp_kvhslice {                 slice(@_, "{", "}", "rv2hv", "padhv")  }

sub pp_lslice
{
    my ($self, $op, $cs) = @_;
    my $idx = $op->first;
    my $list = $op->last;
    my(@elems, $kid);
    my $list_info = $self->deparse($list, 1, $op);
    my $idx_info = $self->deparse($idx, 1, $op);
    return info_from_list($op, $self, ['(', $list_info->{text}, ')', '[', $idx_info->{text}, ']'],
	'', 'lslice', {body=>[$list_info, $idx_info]});
}

sub _method
{
    my($self, $op, $cx) = @_;
    my @other_ops = ($op->first);
    my $kid = $op->first->sibling; # skip pushmark
    my($meth, $obj, @exprs);
    if ($kid->name eq "list" and B::Deparse::want_list $kid) {
	# When an indirect object isn't a bareword but the args are in
	# parens, the parens aren't part of the method syntax (the LLAFR
	# doesn't apply), but they make a list with OPf_PARENS set that
	# doesn't get flattened by the append_elem that adds the method,
	# making a (object, arg1, arg2, ...) list where the object
	# usually is. This can be distinguished from
	# '($obj, $arg1, $arg2)->meth()' (which is legal if $arg2 is an
	# object) because in the later the list is in scalar context
	# as the left side of -> always is, while in the former
	# the list is in list context as method arguments always are.
	# (Good thing there aren't method prototypes!)
	$meth = $kid->sibling;
	push  @other_ops, $kid->first;
	$kid = $kid->first->sibling; # skip pushmark
	$obj = $kid;
	$kid = $kid->sibling;
	for (; not null $kid; $kid = $kid->sibling) {
	    push @exprs, $kid;
	}
    } else {
	$obj = $kid;
	$kid = $kid->sibling;
	for (; !null ($kid->sibling) && $kid->name!~/^method(?:_named)?\z/;
	     $kid = $kid->sibling) {
	    push @exprs, $kid
	}
	$meth = $kid;
    }

    if ($meth->name eq "method_named") {
	$meth = $self->meth_sv($meth)->PV;
    } elsif ($meth->name eq "method_super") {
	$meth = "SUPER::".$self->meth_sv($meth)->PV;
    } elsif ($meth->name eq "method_redir") {
        $meth = $self->meth_rclass_sv($meth)->PV.'::'.$self->meth_sv($meth)->PV;
    } elsif ($meth->name eq "method_redir_super") {
        $meth = $self->meth_rclass_sv($meth)->PV.'::SUPER::'.
                $self->meth_sv($meth)->PV;
    } else {
	$meth = $meth->first;
	if ($meth->name eq "const") {
	    # As of 5.005_58, this case is probably obsoleted by the
	    # method_named case above
	    $meth = $self->const_sv($meth)->PV; # needs to be bare
	}
    }

    return {
	method => $meth,
	variable_method => ref($meth),
	object => $obj,
	args => \@exprs,
	other_ops => \@other_ops
    }, $cx;
}

sub e_method {
    my ($self, $op, $minfo, $cx) = @_;
    my $obj = $self->deparse($minfo->{object}, 24, $op);
    my @body = ($obj);
    my $other_ops = $minfo->{other_ops};

    my $meth = $minfo->{method};
    my $meth_info;
    if ($minfo->{variable_method}) {
	$meth_info = $self->deparse($meth, 1, $op);
	push @body, $meth_info;
    }
    my @args = map { $self->deparse($_, 6, $op) } @{$minfo->{args}};
    push @body, @args;
    my @args_texts = map $_->{text}, @args;
    my $args = join(", ", @args_texts);

    my $opts = {body => \@body, other_ops => $other_ops};
    my @texts = ();
    my $type;

    if ($minfo->{object}->name eq 'scope' && B::Deparse::want_list $minfo->{object}) {
	# method { $object }
	# This must be deparsed this way to preserve list context
	# of $object.
	my $need_paren = $cx >= 6;
	if ($need_paren) {
	    @texts = ('(', $meth,  substr($obj,2),
		      $args, ')');
	    $type = 'e_method_list_paren';
	} else {
	    @texts = ($meth,  substr($obj,2), $args);
	    $type = 'e_method_list';
	}
	return info_from_list($op, $self, \@texts, '', $type, $opts);
    }
    if (length $args) {
	@texts = ($obj->{text}, '->', $meth, '(', $args, ')');
	$type = 'e_method_args';
    } else {
	@texts = ($obj->{text}, '->', $meth);
	$type = 'e_method_null';
    }
    return info_from_list($op, $self, \@texts, '', $type, $opts);
}

# returns "&"  and the argument bodies if the prototype doesn't match the args,
# or ("", $args_after_prototype_demunging) if it does.
sub check_proto {
    my $self = shift;
    my $op = shift;
    return ('&', []) if $self->{'noproto'};
    my($proto, @args) = @_;
    my($arg, $real);
    my $doneok = 0;
    my @reals;
    # An unbackslashed @ or % gobbles up the rest of the args
    1 while $proto =~ s/(?<!\\)([@%])[^\]]+$/$1/;
    $proto =~ s/^\s*//;
    while ($proto) {
	$proto =~ s/^(\\?[\$\@&%*_]|\\\[[\$\@&%*]+\]|;)\s*//;
	my $chr = $1;
	if ($chr eq "") {
	    return ('&', []) if @args;
	} elsif ($chr eq ";") {
	    $doneok = 1;
	} elsif ($chr eq "@" or $chr eq "%") {
	    push @reals, map($self->deparse($_, 6), @args, $op);
	    @args = ();
	} else {
	    $arg = shift @args;
	    last unless $arg;
	    if ($chr eq "\$" || $chr eq "_") {
		if (B::Deparse::want_scalar $arg) {
		    push @reals, $self->deparse($arg, 6, $op);
		} else {
		    return ('&', []);
		}
	    } elsif ($chr eq "&") {
		if ($arg->name =~ /^(s?refgen|undef)$/) {
		    push @reals, $self->deparse($arg, 6, $op);
		} else {
		    return ('&', []);
		}
	    } elsif ($chr eq "*") {
		if ($arg->name =~ /^s?refgen$/
		    and $arg->first->first->name eq "rv2gv")
		  {
		      $real = $arg->first->first; # skip refgen, null
		      if ($real->first->name eq "gv") {
			  push @reals, $self->deparse($real, 6, $op);
		      } else {
			  push @reals, $self->deparse($real->first, 6, $op);
		      }
		  } else {
		      return ('&', []);
		  }
	    } elsif (substr($chr, 0, 1) eq "\\") {
		$chr =~ tr/\\[]//d;
		if ($arg->name =~ /^s?refgen$/ and
		    !null($real = $arg->first) and
		    ($chr =~ /\$/ && B::Deparse::is_scalar($real->first)
		     or ($chr =~ /@/
			 && class($real->first->sibling) ne 'NULL'
			 && $real->first->sibling->name
			 =~ /^(rv2|pad)av$/)
		     or ($chr =~ /%/
			 && class($real->first->sibling) ne 'NULL'
			 && $real->first->sibling->name
			 =~ /^(rv2|pad)hv$/)
		     #or ($chr =~ /&/ # This doesn't work
		     #   && $real->first->name eq "rv2cv")
		     or ($chr =~ /\*/
			 && $real->first->name eq "rv2gv")))
		  {
		      push @reals, $self->deparse($real, 6, $op);
		  } else {
		      return ('&', []);
		  }
	    }
       }
    }
    return ('&', []) if $proto and !$doneok; # too few args and no ';'
    return ('&', []) if @args;               # too many args
    return ('', \@reals);
}

sub pp_enterwrite { unop(@_, "write") }

# Split a floating point number into an integer mantissa and a binary
# exponent. Assumes you've already made sure the number isn't zero or
# some weird infinity or NaN.
sub split_float {
    my($f) = @_;
    my $exponent = 0;
    if ($f == int($f)) {
	while ($f % 2 == 0) {
	    $f /= 2;
	    $exponent++;
	}
    } else {
	while ($f != int($f)) {
	    $f *= 2;
	    $exponent--;
	}
    }
    my $mantissa = sprintf("%.0f", $f);
    return ($mantissa, $exponent);
}

sub dq
{
    my ($self, $op, $parent) = @_;
    my $type = $op->name;
    my $info;
    if ($type eq "const") {
	return info_from_text($op, $self, '$[', 'dq_const_ary', {}) if $op->private & OPpCONST_ARYBASE;
	return info_from_text($op, $self,
			      B::Deparse::uninterp(B::Deparse::escape_str(B::Deparse::unback($self->const_sv($op)->as_string))),
			 'dq_const', {});
    } elsif ($type eq "concat") {
	my $first = $self->dq($op->first, $op);
	my $last  = $self->dq($op->last, $op);

	# Disambiguate "${foo}bar", "${foo}{bar}", "${foo}[1]", "$foo\::bar"
	($last =~ /^[A-Z\\\^\[\]_?]/ &&
	    $first =~ s/([\$@])\^$/${1}{^}/)  # "${^}W" etc
	    || ($last =~ /^[:'{\[\w_]/ && #'
		$first =~ s/([\$@])([A-Za-z_]\w*)$/${1}{$2}/);

	return info_from_list($op, $self, [$first->{text}, $last->{text}], '', 'dq_concat',
			      {body => [$first, $last]});
    } elsif ($type eq "join") {
	return $self->deparse($op->last, 26, $op); # was join($", @ary)
    } else {
	return $self->deparse($op, 26, $parent);
    }
    my $kid = $self->dq($op->first->sibling, $op);
    my $kid_text = $kid->{text};
    if ($type eq "uc") {
	$info = info_from_lists(['\U', $kid, '\E'], '', 'dq_uc', {});
    } elsif ($type eq "lc") {
	$info = info_from_lists(['\L', $kid, '\E'], '', 'dq_lc', {});
    } elsif ($type eq "ucfirst") {
	$info = info_from_lists(['\u', $kid, '\E'], '', 'dq_ucfirst', {});
    } elsif ($type eq "lcfirst") {
	$info = info_from_lists(['\l', $kid, '\E'], '', 'dq_lcfirst', {});
    } elsif ($type eq "quotemeta") {
	$info = info_from_lists(['\Q', $kid, '\E'], '', 'dq_quotemeta', {});
    } elsif ($type eq "fc") {
	$info = info_from_lists(['\F', $kid, '\E'], '', 'dq_fc', {});
    }
    $info->{body} = [$kid];
    return $info;
}

sub pp_backtick
{
    my($self, $op, $cx) = @_;
    # skip pushmark if it exists (readpipe() vs ``)
    my $child = $op->first->sibling->isa('B::NULL')
	? $op->first : $op->first->sibling;
    if ($self->pure_string($child)) {
	return $self->single_delim($op, "qx", '`', $self->dq($child, 1)->{text});
    }
    unop($self, $op, $cx, "readpipe");
}

# OP_STRINGIFY is a listop, but it only ever has one arg
sub pp_stringify {
    my ($self, $op, $cx) = @_;
    my $kid = $op->first->sibling;
    my @other_ops = ();
    while ($kid->name eq 'null' && !null($kid->first)) {
        push(@other_ops, $kid);
	$kid = $kid->first;
    }
    my $info;
    if ($kid->name =~ /^(?:const|padsv|rv2sv|av2arylen|gvsv|multideref
			  |aelemfast(?:_lex)?|[ah]elem|join|concat)\z/x) {
	$info = maybe_targmy(@_, \&dquote);
    }
    else {
	# Actually an optimised join.
	my $info = listop(@_,"join");
	$info->{text} =~ s/join([( ])/join$1$self->{'ex_const'}, /;
    }
    push @{$info->{other_ops}}, @other_ops;
    return $info;
}


# tr/// and s/// (and tr[][], tr[]//, tr###, etc)
# note that tr(from)/to/ is OK, but not tr/from/(to)
sub double_delim {
    my($from, $to) = @_;
    my($succeed, $delim);
    if ($from !~ m[/] and $to !~ m[/]) {
	return "/$from/$to/";
    } elsif (($succeed, $from) = B::Deparse::balanced_delim($from) and $succeed) {
	if (($succeed, $to) = B::Deparse::balanced_delim($to) and $succeed) {
	    return "$from$to";
	} else {
	    for $delim ('/', '"', '#') { # note no "'" -- s''' is special
		return "$from$delim$to$delim" if index($to, $delim) == -1;
	    }
	    $to =~ s[/][\\/]g;
	    return "$from/$to/";
	}
    } else {
	for $delim ('/', '"', '#') { # note no '
	    return "$delim$from$delim$to$delim"
		if index($to . $from, $delim) == -1;
	}
	$from =~ s[/][\\/]g;
	$to =~ s[/][\\/]g;
	return "/$from/$to/";
    }
}

# Only used by tr///, so backslashes hyphens
sub pchr { # ASCII
    my($n) = @_;
    if ($n == ord '\\') {
	return '\\\\';
    } elsif ($n == ord "-") {
	return "\\-";
    } elsif ($n >= ord(' ') and $n <= ord('~')) {
	return chr($n);
    } elsif ($n == ord "\a") {
	return '\\a';
    } elsif ($n == ord "\b") {
	return '\\b';
    } elsif ($n == ord "\t") {
	return '\\t';
    } elsif ($n == ord "\n") {
	return '\\n';
    } elsif ($n == ord "\e") {
	return '\\e';
    } elsif ($n == ord "\f") {
	return '\\f';
    } elsif ($n == ord "\r") {
	return '\\r';
    } elsif ($n >= ord("\cA") and $n <= ord("\cZ")) {
	return '\\c' . chr(ord("@") + $n);
    } else {
	# return '\x' . sprintf("%02x", $n);
	return '\\' . sprintf("%03o", $n);
    }
}

sub collapse {
    my(@chars) = @_;
    my($str, $c, $tr) = ("");
    for ($c = 0; $c < @chars; $c++) {
	$tr = $chars[$c];
	$str .= pchr($tr);
	if ($c <= $#chars - 2 and $chars[$c + 1] == $tr + 1 and
	    $chars[$c + 2] == $tr + 2)
	{
	    for (; $c <= $#chars-1 and $chars[$c + 1] == $chars[$c] + 1; $c++)
	      {}
	    $str .= "-";
	    $str .= pchr($chars[$c]);
	}
    }
    return $str;
}

sub tr_decode_byte {
    my($table, $flags) = @_;
    my(@table) = unpack("s*", $table);
    splice @table, 0x100, 1;   # Number of subsequent elements
    my($c, $tr, @from, @to, @delfrom, $delhyphen);
    if ($table[ord "-"] != -1 and
	$table[ord("-") - 1] == -1 || $table[ord("-") + 1] == -1)
    {
	$tr = $table[ord "-"];
	$table[ord "-"] = -1;
	if ($tr >= 0) {
	    @from = ord("-");
	    @to = $tr;
	} else { # -2 ==> delete
	    $delhyphen = 1;
	}
    }
    for ($c = 0; $c < @table; $c++) {
	$tr = $table[$c];
	if ($tr >= 0) {
	    push @from, $c; push @to, $tr;
	} elsif ($tr == -2) {
	    push @delfrom, $c;
	}
    }
    @from = (@from, @delfrom);
    if ($flags & OPpTRANS_COMPLEMENT) {
	my @newfrom = ();
	my %from;
	@from{@from} = (1) x @from;
	for ($c = 0; $c < 256; $c++) {
	    push @newfrom, $c unless $from{$c};
	}
	@from = @newfrom;
    }
    unless ($flags & OPpTRANS_DELETE || !@to) {
	pop @to while $#to and $to[$#to] == $to[$#to -1];
    }
    my($from, $to);
    $from = collapse(@from);
    $to = collapse(@to);
    $from .= "-" if $delhyphen;
    return ($from, $to);
}

# XXX This doesn't yet handle all cases correctly either

sub tr_decode_utf8 {
    my($swash_hv, $flags) = @_;
    my %swash = $swash_hv->ARRAY;
    my $final = undef;
    $final = $swash{'FINAL'}->IV if exists $swash{'FINAL'};
    my $none = $swash{"NONE"}->IV;
    my $extra = $none + 1;
    my(@from, @delfrom, @to);
    my $line;
    foreach $line (split /\n/, $swash{'LIST'}->PV) {
	my($min, $max, $result) = split(/\t/, $line);
	$min = hex $min;
	if (length $max) {
	    $max = hex $max;
	} else {
	    $max = $min;
	}
	$result = hex $result;
	if ($result == $extra) {
	    push @delfrom, [$min, $max];
	} else {
	    push @from, [$min, $max];
	    push @to, [$result, $result + $max - $min];
	}
    }
    for my $i (0 .. $#from) {
	if ($from[$i][0] == ord '-') {
	    unshift @from, splice(@from, $i, 1);
	    unshift @to, splice(@to, $i, 1);
	    last;
	} elsif ($from[$i][1] == ord '-') {
	    $from[$i][1]--;
	    $to[$i][1]--;
	    unshift @from, ord '-';
	    unshift @to, ord '-';
	    last;
	}
    }
    for my $i (0 .. $#delfrom) {
	if ($delfrom[$i][0] == ord '-') {
	    push @delfrom, splice(@delfrom, $i, 1);
	    last;
	} elsif ($delfrom[$i][1] == ord '-') {
	    $delfrom[$i][1]--;
	    push @delfrom, ord '-';
	    last;
	}
    }
    if (defined $final and $to[$#to][1] != $final) {
	push @to, [$final, $final];
    }
    push @from, @delfrom;
    if ($flags & OPpTRANS_COMPLEMENT) {
	my @newfrom;
	my $next = 0;
	for my $i (0 .. $#from) {
	    push @newfrom, [$next, $from[$i][0] - 1];
	    $next = $from[$i][1] + 1;
	}
	@from = ();
	for my $range (@newfrom) {
	    if ($range->[0] <= $range->[1]) {
		push @from, $range;
	    }
	}
    }
    my($from, $to, $diff);
    for my $chunk (@from) {
	$diff = $chunk->[1] - $chunk->[0];
	if ($diff > 1) {
	    $from .= tr_chr($chunk->[0]) . "-" . tr_chr($chunk->[1]);
	} elsif ($diff == 1) {
	    $from .= tr_chr($chunk->[0]) . tr_chr($chunk->[1]);
	} else {
	    $from .= tr_chr($chunk->[0]);
	}
    }
    for my $chunk (@to) {
	$diff = $chunk->[1] - $chunk->[0];
	if ($diff > 1) {
	    $to .= tr_chr($chunk->[0]) . "-" . tr_chr($chunk->[1]);
	} elsif ($diff == 1) {
	    $to .= tr_chr($chunk->[0]) . tr_chr($chunk->[1]);
	} else {
	    $to .= tr_chr($chunk->[0]);
	}
    }
    #$final = sprintf("%04x", $final) if defined $final;
    #$none = sprintf("%04x", $none) if defined $none;
    #$extra = sprintf("%04x", $extra) if defined $extra;
    #print STDERR "final: $final\n none: $none\nextra: $extra\n";
    #print STDERR $swash{'LIST'}->PV;
    return (B::Deparse::escape_str($from), B::Deparse::escape_str($to));
}

sub pp_trans {
    my $self = shift;
    my($op, $cx) = @_;
    my($from, $to);
    my $class = class($op);
    my $priv_flags = $op->private;
    if ($class eq "PVOP") {
	($from, $to) = tr_decode_byte($op->pv, $priv_flags);
    } elsif ($class eq "PADOP") {
	($from, $to)
	  = tr_decode_utf8($self->padval($op->padix)->RV, $priv_flags);
    } else { # class($op) eq "SVOP"
	($from, $to) = tr_decode_utf8($op->sv->RV, $priv_flags);
    }
    my $flags = "";
    $flags .= "c" if $priv_flags & OPpTRANS_COMPLEMENT;
    $flags .= "d" if $priv_flags & OPpTRANS_DELETE;
    $to = "" if $from eq $to and $flags eq "";
    $flags .= "s" if $priv_flags & OPpTRANS_SQUASH;
    return info_from_list($op, $self, ['tr', double_delim($from, $to), $flags],
		      '', 'pp_trans', {});
}

sub pp_transr {
    my $self = $_[0];
    my $op = $_[1];
    my $info = pp_trans(@_);
    return info_from_text($op, $self, $info->{text} . 'r', 'pp_transr',
			  {body => [$info]});
}

sub re_dq_disambiguate {
    my ($first, $last) = @_;
    # Disambiguate "${foo}bar", "${foo}{bar}", "${foo}[1]"
    ($last =~ /^[A-Z\\\^\[\]_?]/ &&
	$first =~ s/([\$@])\^$/${1}{^}/)  # "${^}W" etc
	|| ($last =~ /^[{\[\w_]/ &&
	    $first =~ s/([\$@])([A-Za-z_]\w*)$/${1}{$2}/);
    return $first . $last;
}

# Like dq(), but different
sub re_dq {
    my $self = shift;
    my ($op) = @_;
    my ($re_dq_info, $fmt);

    my $type = $op->name;
    if ($type eq "const") {
	return '$[' if $op->private & OPpCONST_ARYBASE;
	my $unbacked = B::Deparse::re_unback($self->const_sv($op)->as_string);
	return B::Deparse::re_uninterp(escape_re($unbacked));
    } elsif ($type eq "concat") {
	my $first = $self->re_dq($op->first);
	my $last  = $self->re_dq($op->last);
	return re_dq_disambiguate($first, $last);
    } elsif ($type eq "uc") {
	$re_dq_info = $self->re_dq($op->first->sibling);
	$fmt = '\U%c\E';
	$type .= ' uc';
    } elsif ($type eq "lc") {
	$re_dq_info = $self->re_dq($op->first->sibling);
	$fmt = '\L%c\E';
	$type .= ' lc';
    } elsif ($type eq "ucfirst") {
	$re_dq_info = $self->re_dq($op->first->sibling);
	$fmt = '\u%c';
	$type .= ' ucfirst';
    } elsif ($type eq "lcfirst") {
	$re_dq_info = $self->re_dq($op->first->sibling);
	$fmt = '\u%c';
	$type .= ' lcfirst';
    } elsif ($type eq "quotemeta") {
	$re_dq_info = $self->re_dq($op->first->sibling);
	$fmt = '\Q%c\E';
	$type .= ' quotemeta';
    } elsif ($type eq "fc") {
	$re_dq_info = $self->re_dq($op->first->sibling);
	$fmt = '\F%c\E';
	$type .= ' fc';
    } elsif ($type eq "join") {
	return $self->deparse($op->last, 26); # was join($", @ary)
    } else {
	my $ret = $self->deparse($op, 26);
	$ret =~ s/^\$([(|)])\z/\${$1}/ # $( $| $) need braces
	or $ret =~ s/^\@([-+])\z/\@{$1}/; # @- @+ need braces
	return $ret;
    }
    return $self->info_from_template($type, $op->first->sibling,
				     $fmt, [$re_dq_info], [0]);
}

sub pure_string {
    my ($self, $op) = @_;
    return 0 if null $op;
    my $type = $op->name;

    if ($type eq 'const' || $type eq 'av2arylen') {
	return 1;
    }
    elsif ($type =~ /^(?:[ul]c(first)?|fc)$/ || $type eq 'quotemeta') {
	return $self->pure_string($op->first->sibling);
    }
    elsif ($type eq 'join') {
	my $join_op = $op->first->sibling;  # Skip pushmark
	return 0 unless $join_op->name eq 'null' && $join_op->targ == OP_RV2SV;

	my $gvop = $join_op->first;
	return 0 unless $gvop->name eq 'gvsv';
        return 0 unless '"' eq $self->gv_name($self->gv_or_padgv($gvop));

	return 0 unless ${$join_op->sibling} eq ${$op->last};
	return 0 unless $op->last->name =~ /^(?:[ah]slice|(?:rv2|pad)av)$/;
    }
    elsif ($type eq 'concat') {
	return $self->pure_string($op->first)
            && $self->pure_string($op->last);
    }
    elsif (B::Deparse::is_scalar($op) || $type =~ /^[ah]elem$/) {
	return 1;
    }
    elsif ($type eq "null" and $op->can('first') and not null $op->first and
	  ($op->first->name eq "null" and $op->first->can('first')
	   and not null $op->first->first and
	   $op->first->first->name eq "aelemfast"
          or
	   $op->first->name =~ /^aelemfast(?:_lex)?\z/
	  )) {
	return 1;
    }
    else {
	return 0;
    }

    return 1;
}

sub regcomp
{
    my($self, $op, $cx, $extended) = @_;
    my @other_ops = ();
    my $kid = $op->first;
    if ($kid->name eq "regcmaybe") {
	push @other_ops, $kid;
	$kid = $kid->first;
    }
    if ($kid->name eq "regcreset") {
	push @other_ops, $kid;
	$kid = $kid->first;
    }
    if ($kid->name eq "null" and !null($kid->first)
	and $kid->first->name eq 'pushmark') {
	my $str = '';
	push(@other_ops, $kid);
	$kid = $kid->first->sibling;
	my @body = ();
	while (!null($kid)) {
	    my $first = $str;
	    my $last = $self->re_dq($kid, $extended);
	    push @body, $last;
	    push(@other_ops, $kid);
	    $str = re_dq_disambiguate($first,
				      $self->info2str($last));
	    $kid = $kid->sibling;
	}
	return (info_from_text($op, $self, $str, 'regcomp',
			       {other_ops => \@other_ops,
				body => \@body}), 1);
    }

    if ($self->pure_string($kid)) {
	my $info = $self->re_dq($kid, $extended);
	my @kid_ops = $info->{other_ops} ? @{$info->{other_ops}} : ();
	push @other_ops, @kid_ops;
	$info->{other_ops} = \@other_ops;
	return ($info, 1);
    }
    return ($self->deparse($kid, $cx, $op), 0, $op);
}

# osmic acid -- see osmium tetroxide

sub pp_match { matchop(@_, "m", "/") }
sub pp_pushre { matchop(@_, "m", "/") }
sub pp_qr { matchop(@_, "qr", "") }

sub pp_runcv { unop(@_, "__SUB__"); }

sub pp_split
{
    my($self, $op, $cx) = @_;
    my($kid, @exprs, $ary_info, $expr);
    my $stacked = $op->flags & OPf_STACKED;
    my $ary = '';
    my @body = ();
    my @other_ops = ();
    $kid = $op->first;

    $kid = $op->first;
    $kid = $kid->sibling if $kid->name eq 'regcomp';
    for (; !null($kid); $kid = $kid->sibling) {
	push @exprs, $self->deparse($kid, 6, $op);
    }

    unshift @exprs, $self->matchop($op, $cx, "m", "/");

    if ($op->private & OPpSPLIT_ASSIGN) {
        # With C<@array = split(/pat/, str);>,
        #  array is stored in split's pmreplroot; either
        # as an integer index into the pad (for a lexical array)
        # or as GV for a package array (which will be a pad index
        # on threaded builds)
        # With my/our @array = split(/pat/, str), the array is instead
        # accessed via an extra padav/rv2av op at the end of the
        # split's kid ops.

        if ($stacked) {
            $ary = pop @exprs;
        }
        else {
            if ($op->private & OPpSPLIT_LEX) {
                $ary = $self->padname($op->pmreplroot);
            }
            else {
                # union with op_pmtargetoff, op_pmtargetgv
                my $gv = $op->pmreplroot;
                $gv = $self->padval($gv) if !ref($gv);
                $ary = $self->maybe_local(@_,
			      $self->stash_variable('@',
						     $self->gv_name($gv),
						     $cx))
            }
            if ($op->private & OPpLVAL_INTRO) {
                $ary = $op->private & OPpSPLIT_LEX ? "my $ary" : "local $ary";
            }
        }
    }

    push @body, @exprs;
    my $opts = {body => \@exprs};

    # handle special case of split(), and split(' ') that compiles to /\s+/
    if (($op->reflags // 0) & RXf_SKIPWHITE()) {
	my $expr0 = $exprs[0];
	my $expr0b0 = $expr0->{body}[0];
	my $bsep = $expr0b0->{sep};
	my $sep = $expr0->{sep};
	$expr0b0->{texts}[1] = ' ';
	# substr($expr0b0->{text}, 1, 0) = ' ';
	substr($expr0->{texts}[0], 1, 0) = ' ';
	substr($expr0->{text}, 1, 0) = ' ';
    }
    my @args_texts = map $_->{text}, @exprs;

    my $sep = '';
    my $type;
    my @expr_texts;
    if ($ary) {
	@expr_texts = ("$ary", '=', join(', ', @args_texts));
	$sep = ' ';
	$type = 'split_array';
	$opts->{maybe_parens} = [$self, $cx, 7];
    } else {
	@expr_texts = ('split', '(', join(', ', @args_texts), ')');
	$type = 'split';

    }
    return info_from_list($op, $self, \@expr_texts, $sep, $type, $opts);
}

# Kind of silly, but we prefer, subst regexp flags joined together to
# make words. For example: s/a/b/xo => s/a/b/ox

# oxime -- any of various compounds obtained chiefly by the action of
# hydroxylamine on aldehydes and ketones and characterized by the
# bivalent grouping C=NOH [Webster's Tenth]

my %substwords;
map($substwords{join "", sort split //, $_} = $_, 'ego', 'egoism', 'em',
    'es', 'ex', 'exes', 'gee', 'go', 'goes', 'ie', 'ism', 'iso', 'me',
    'meese', 'meso', 'mig', 'mix', 'os', 'ox', 'oxime', 'see', 'seem',
    'seg', 'sex', 'sig', 'six', 'smog', 'sog', 'some', 'xi', 'rogue',
    'sir', 'rise', 'smore', 'more', 'seer', 'rome', 'gore', 'grim', 'grime',
    'or', 'rose', 'rosie');

sub pp_subst
{
    my($self, $op, $cx) = @_;
    my $kid = $op->first;
    my($binop, $var, $re, @other_ops) = ("", "", "", ());
    my @body = ();
    my ($repl, $repl_info);

    if ($op->flags & OPf_STACKED) {
	$binop = 1;
	$var = $self->deparse($kid, 20, $op);
	$kid = $kid->sibling;
    }
    elsif (my $targ = $op->targ) {
	$binop = 1;
	$var = $self->padname($targ);
    }
    my $flags = "";
    my $pmflags = $op->pmflags;
    if (null($op->pmreplroot)) {
	$repl = $kid;
	$kid = $kid->sibling;
    } else {
	push @other_ops, $op->pmreplroot;
	$repl = $op->pmreplroot->first; # skip substcont
    }
    while ($repl->name eq "entereval") {
	push @other_ops, $repl;
	$repl = $repl->first;
	    $flags .= "e";
    }
    {
	local $self->{in_subst_repl} = 1;
	if ($pmflags & PMf_EVAL) {
	    $repl_info = $self->deparse($repl->first, 0, $repl);
	} else {
	    $repl_info = $self->dq($repl);
	}
    }
    if (not null my $code_list = $op->code_list) {
	$re = $self->code_list($code_list);
    } elsif (null $kid) {
	$re = B::Deparse::re_uninterp(escape_re(B::Deparse::re_unback($op->precomp)));
    } else {
	my ($re_info, $junk) = $self->regcomp($kid, 1);
	push @body, $re_info;
	$re = $re_info->{text};
    }
    $flags .= "r" if $pmflags & PMf_NONDESTRUCT;
    $flags .= "e" if $pmflags & PMf_EVAL;
    $flags .= $self->re_flags($op);
    $flags = join '', sort split //, $flags;
    $flags = $substwords{$flags} if $substwords{$flags};
    my $core_s = $self->keyword("s"); # maybe CORE::s
    my $info;
    my $repl_text = $repl_info->{text};
    my $opts->{other_ops} = \@other_ops if @other_ops;
    my $find_replace_re = double_delim($re, $repl_text);
    my $args = [$var, $find_replace_re];
    my $args_spec = [0, 1];
    if ($binop) {
	my $fmt = "%c =~ $core_s%c$flags";
	return $self->info_from_template("=~ s///", $op, $fmt, $args_spec, $args,
					 {maybe_parens => [$self, $cx, 20]});
    } else {
	my $fmt = "$core_s%c$flags";
	return $self->info_from_template("s///", $op, $fmt, $args_spec, $args, {});
    }
    Carp::confess("unhandled condition in pp_subst");
}

unless (caller) {
    eval "use Data::Printer;";

    eval {
	sub fib($) {
	    my $x = shift;
	    return 1 if $x <= 1;
	    return(fib($x-1) + fib($x-2))
	}
	sub baz {
	    no strict;
	    CORE::wait;
	}
    };

    # use B::Deparse;
    # my $deparse_old = B::Deparse->new("-l", "-sC");
    # print $deparse_old->coderef2text(\&baz);
    # exit 1;
    my $deparse = __PACKAGE__->new("-l", "-c", "-sC");
    my $info = $deparse->coderef2info(\&baz);
    import Data::Printer colored => 0;
    Data::Printer::p($info);
    print "\n", '=' x 30, "\n";
    # print $deparse->($deparse->deparse_subname('fib')->{text});
    # print "\n", '=' x 30, "\n";
    # print "\n", '-' x 30, "\n";
    while (my($key, $value) = each %{$deparse->{optree}}) {
	my $parent_op_name = 'undef';
	if ($value->{parent}) {
	    my $parent = $deparse->{optree}{$value->{parent}};
	    $parent_op_name = $parent->{op}->name if $parent->{op};
	}
	printf("0x%x %s/%s of %s |\n%s",
	       $key, $value->{op}->name, $value->{type},
	       $parent_op_name, $deparse->indent($value->{text}));
	printf " ## line %s\n", $value->{cop} ? $value->{cop}->line : 'undef';
	print '-' x 30, "\n";
    }
}

# Not in Perl 5.20 and presumably < 5.20. No harm in adding to 5.20?
*pp_ncomplement = *pp_complement;
sub pp_scomplement { maybe_targmy(@_, \&pfixop, "~.", 21) }

unless (caller) {
    eval "use Data::Printer;";

    eval {
	our($Fileparse_fstype);
	sub fib($) {
	    my $x = shift;
	    return 1 if $x <= 1;
	    return(fib($x-1) + fib($x-2))
	}
	sub fileparse {
	    no strict;
  # my($fullname,@suffices) = @_;

  my $tail   = '';
  $tail = $1 . $tail;

  # Ensure taint is propagated from the path to its pieces.
  $tail .= $taint;
  wantarray ? ($basename .= $taint, $dirpath .= $taint, $tail)
            : ($basename .= $taint);
}
	sub baz {
	    no strict;
	    if ($basename =~ s/$pat//s) {
	    }
	}
    };

    my $deparse = __PACKAGE__->new("-l", "-c");
    my $info = $deparse->coderef2info(\&fileparse);
    # my $info = $deparse->coderef2info(\&baz);
    import Data::Printer colored => 0;
    Data::Printer::p($info);
    print "\n", '=' x 30, "\n";
    # print $deparse->indent($deparse->deparse_subname('fib')->{text});
    # print "\n", '=' x 30, "\n";
    # print "\n", '-' x 30, "\n";
    while (my($key, $value) = each %{$deparse->{optree}}) {
	my $parent_op_name = 'undef';
	if ($value->{parent}) {
	    my $parent = $deparse->{optree}{$value->{parent}};
	    $parent_op_name = $parent->{op}->name if $parent->{op};
	}
	if (eval{$value->{op}->name}) {
	    printf("0x%x %s/%s of %s |\n%s",
		   $key, $value->{op}->name, $value->{type},
		   $parent_op_name, $deparse->{text});
	} else {
	    printf("0x%x %s of %s |\n",
		   $key, $value->{type},
		   $parent_op_name);
	}
	printf " ## line %s\n", $value->{cop} ? $value->{cop}->line : 'undef';
	print '-' x 30, "\n";
    }
    # use B::Deparse;
    # my $deparse_old = B::Deparse->new("-l", "-sC");
    # print $deparse_old->coderef2text(\&baz);
}

1;
