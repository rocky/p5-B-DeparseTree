# Common routines used by PP Functions
# Copyright (c) 2015-2018 Rocky Bernstein
# Copyright (c) 1998-2000, 2002, 2003, 2004, 2005, 2006 Stephen McCamant.

# All rights reserved.
# This module is free software; you can redistribute and/or modify
# it under the same terms as Perl itself.

# This is based on the module B::Deparse by Stephen McCamant.
# It has been extended save tree structure, and is addressible
# by opcode address.

# B::Parse in turn is based on the module of the same name by Malcolm Beattie,
# but essentially none of his code remains.
use strict; use warnings;

package B::DeparseTree::PPfns;
use Carp;
use B qw(
         OPf_STACKED
         OPpCONST_BARE
         OPpLVAL_INTRO
         OPpSORT_INTEGER
         OPpSORT_NUMERIC
         OPpSORT_REVERSE
    );

use B::DeparseTree::Common;
use B::DeparseTree::SyntaxTree;

our($VERSION, @EXPORT, @ISA);
$VERSION = '3.1.1';
@ISA = qw(Exporter);
@EXPORT = qw(
    POSTFIX
    baseop
    binop
    deparse_binop_left
    deparse_binop_right
    dq_unop
    givwhen
    indirop
    listop
    logop
    loop_common
    loopex
    mapop
    matchop
    pfixop
    unop
    );

BEGIN {
    # List version-specific constants here.
    # Easiest way to keep this code portable between version looks to
    # be to fake up a dummy constant that will never actually be true.
    foreach (qw(OPpSORT_INPLACE OPpSORT_DESCEND OPpITER_REVERSED
                OPpCONST_NOVER OPpPAD_STATE PMf_SKIPWHITE RXf_SKIPWHITE
		RXf_PMf_CHARSET RXf_PMf_KEEPCOPY
		CVf_LOCKED OPpREVERSE_INPLACE OPpSUBSTR_REPL_FIRST
		PMf_NONDESTRUCT OPpCONST_ARYBASE OPpEVAL_BYTES)) {
	eval { import B $_ };
	no strict 'refs';
	*{$_} = sub () {0} unless *{$_}{CODE};
    }
}

# routines implementing classes of ops

sub baseop
{
    my($self, $op, $cx, $name) = @_;
    return $self->info_from_string("baseop $name", $op, $self->keyword($name));
}

sub binop
{

    my ($self, $op, $cx, $opname, $prec) = @_;
    my ($flags, $type) = (0, '');
    if (scalar(@_) > 5) {
	$flags = $_[5];
	$type = $_[6] if (scalar(@_) > 6);
    }
    my $left = $op->first;
    my $right = $op->last;
    my $eq = "";
    if ($op->flags & B::OPf_STACKED && $flags & B::Deparse::ASSIGN) {
	$eq = "=";
	$prec = 7;
    }
    if ($flags & B::Deparse::SWAP_CHILDREN) {
	($left, $right) = ($right, $left);
    }
    my $lhs = $self->deparse_binop_left($op, $left, $prec);
    if ($flags & B::Deparse::LIST_CONTEXT
	&& $lhs->{text} !~ /^(my|our|local|)[\@\(]/) {
	$lhs->{text} = "($lhs->{text})";
    }

    my $rhs = $self->deparse_binop_right($op, $right, $prec);
    my @texts = ($lhs, $rhs);

    $type = $type || 'binary_operator';
    $type .= " $opname$eq";
    return $self->info_from_template($type, $op, "%c $opname$eq %c", undef, \@texts,
				     {maybe_parens => [$self, $cx, $prec]});
}

my(%left, %right);

sub assoc_class {
    my $op = shift;
    my $name = $op->name;
    if ($name eq "concat" and $op->first->name eq "concat") {
	# avoid spurious '=' -- see comment in pp_concat
	return "concat";
    }
    if ($name eq "null" and B::class($op) eq "UNOP"
	and $op->first->name =~ /^(and|x?or)$/
	and null $op->first->sibling)
    {
	# Like all conditional constructs, OP_ANDs and OP_ORs are topped
	# with a null that's used as the common end point of the two
	# flows of control. For precedence purposes, ignore it.
	# (COND_EXPRs have these too, but we don't bother with
	# their associativity).
	return assoc_class($op->first);
    }
    return $name . ($op->flags & B::OPf_STACKED ? "=" : "");
}

# Left associative operators, like '+', for which
# $a + $b + $c is equivalent to ($a + $b) + $c

BEGIN {
    %left = ('multiply' => 19, 'i_multiply' => 19,
	     'divide' => 19, 'i_divide' => 19,
	     'modulo' => 19, 'i_modulo' => 19,
	     'repeat' => 19,
	     'add' => 18, 'i_add' => 18,
	     'subtract' => 18, 'i_subtract' => 18,
	     'concat' => 18,
	     'left_shift' => 17, 'right_shift' => 17,
	     'bit_and' => 13,
	     'bit_or' => 12, 'bit_xor' => 12,
	     'and' => 3,
	     'or' => 2, 'xor' => 2,
	    );
}

sub deparse_binop_left {
    my $self = shift;
    my($op, $left, $prec) = @_;
    if ($left{assoc_class($op)} && $left{assoc_class($left)}
	and $left{assoc_class($op)} == $left{assoc_class($left)})
    {
	return $self->deparse($left, $prec - .00001, $op);
    } else {
	return $self->deparse($left, $prec, $op);
    }
}

# Right associative operators, like '=', for which
# $a = $b = $c is equivalent to $a = ($b = $c)

BEGIN {
    %right = ('pow' => 22,
	      'sassign=' => 7, 'aassign=' => 7,
	      'multiply=' => 7, 'i_multiply=' => 7,
	      'divide=' => 7, 'i_divide=' => 7,
	      'modulo=' => 7, 'i_modulo=' => 7,
	      'repeat=' => 7,
	      'add=' => 7, 'i_add=' => 7,
	      'subtract=' => 7, 'i_subtract=' => 7,
	      'concat=' => 7,
	      'left_shift=' => 7, 'right_shift=' => 7,
	      'bit_and=' => 7,
	      'bit_or=' => 7, 'bit_xor=' => 7,
	      'andassign' => 7,
	      'orassign' => 7,
	     );
}

sub deparse_binop_right {
    my $self = shift;
    my($op, $right, $prec) = @_;
    if ($right{assoc_class($op)} && $right{assoc_class($right)}
	and $right{assoc_class($op)} == $right{assoc_class($right)})
    {
	return $self->deparse($right, $prec - .00001, $op);
    } else {
	return $self->deparse($right, $prec, $op);
    }
}

# Unary operators that can occur as pseudo-listops inside double quotes
sub dq_unop
{
    my($self, $op, $cx, $name, $prec, $flags) = (@_, 0, 0);
    my $kid;
    if ($op->flags & B::OPf_KIDS) {
	my $skipped_ops = undef;
	$kid = $op->first;
	if (not B::Deparse::null $kid->sibling) {
	    # If there's more than one kid, the first is an ex-pushmark.
	    $skipped_ops = [$kid];
	    $kid = $kid->sibling;
	}
	my $info = $self->maybe_parens_unop($name, $kid, $cx, $op);
	$info->{other_ops} = $skipped_ops if $skipped_ops;
	return $info;
    } else {
	my @texts = ($name);
	push @texts, '(', ')' if $op->flags & B::OPf_SPECIAL;
	return info_from_list($op, $self, \@texts, '', 'dq', {});
    }
    Carp::confess("unhandled condition in dq_unop");
}

sub givwhen
{
    my($self, $op, $cx, $give_when) = @_;

    my @arg_spec = ();
    my @nodes = ();
    my $enterop = $op->first;
    my $fmt;
    my ($head, $block);
    if ($enterop->flags & B::OPf_SPECIAL) {
	$head = $self->keyword("default");
	$fmt = "$give_when ($head)\n\%+%c\n%-}\n";
	$block = $self->deparse($enterop->first, 0, $enterop, $op);
    }
    else {
	my $cond = $enterop->first;
	my $cond_node = $self->deparse($cond, 1, $enterop, $op);
	push @nodes, $cond_node;
	$fmt = "$give_when (%c)\n\%+%c\n%-}\n";
	$block = $self->deparse($cond->sibling, 0, $enterop, $op);
    }
    push @nodes, $block;

    return $self->info_from_template("{} $give_when",
				     "%c\n\%+%c\n%-}\n", [0, 1],
				     \@nodes);
}

# Logical ops, if/until, &&, and
# The one-line while/until is handled in pp_leave
sub logop
{
    my ($self, $op, $cx, $lowop, $lowprec, $highop,
	$highprec, $blockname) = @_;
    my $left = $op->first;
    my $right = $op->first->sibling;
    my ($lhs, $rhs, $type, $opname);
    my $opts = {};
    if ($cx < 1 and B::Deparse::is_scope($right) and $blockname
	and $self->{'expand'} < 7) {
	# Is this branch used in 5.26 and above?
	# <if> ($a) {$b}
	my $if_cond_info = $self->deparse($left, 1, $op);
	my $if_body_info = $self->deparse($right, 0, $op);
	return $self->info_from_template("$blockname () {}", $op,
					 "$blockname (%c) {\n%+%c\n%-}",
					 [0, 1],
					 [$if_cond_info, $if_body_info], $opts);
    } elsif ($cx < 1 and $blockname and not $self->{'parens'}
	     and $self->{'expand'} < 7) { # $b if $a
	# Note: order of lhs and rhs is reversed
	$lhs = $self->deparse($right, 1, $op);
	$rhs = $self->deparse($left, 1, $op);
	$opname = $blockname;
	$type = "suffix $opname"
    } elsif ($cx > $lowprec and $highop) {
	# low-precedence operator like $a && $b
	$lhs = $self->deparse_binop_left($op, $left, $highprec);
	$rhs = $self->deparse_binop_right($op, $right, $highprec);
	$opname = $highop;
	$opts = {maybe_parens => [$self, $cx, $highprec]};
    } else {
	# high-precedence operator like $a and $b
	$lhs = $self->deparse_binop_left($op, $left, $lowprec);
	$rhs = $self->deparse_binop_right($op, $right, $lowprec);
	$opname = $lowop;
	$opts = {maybe_parens => [$self, $cx, $lowprec]};
    }
    $type ||= $opname;
    return $self->info_from_template($type, $op, "%c $opname %c",
				     [0, 1], [$lhs, $rhs], $opts);
}

sub listop
{
    my($self, $op, $cx, $name, $kid, $nollafr) = @_;
    my(@exprs, @new_nodes, @skipped_ops);
    my $parens = ($cx >= 5) || $self->{'parens'};

    unless ($kid) {
	push @skipped_ops, $op->first;
	$kid = $op->first->sibling;
    }

    # If there are no arguments, add final parentheses (or parenthesize the
    # whole thing if the llafr does not apply) to account for cases like
    # (return)+1 or setpgrp()+1.  When the llafr does not apply, we use a
    # precedence of 6 (< comma), as "return, 1" does not need parentheses.
    if (B::Deparse::null $kid) {
	my $fullname = $self->keyword($name);
	my $text = $nollafr
	    ? $self->maybe_parens($fullname, $cx, 7)
	    : $fullname . '()' x (7 < $cx);
	return $self->info_from_string("listop $name", $op, $text);
    }
    my $first;
    my $fullname = $self->keyword($name);
    my $proto = prototype("CORE::$name");
    if (
	 (     (defined $proto && $proto =~ /^;?\*/)
	    || $name eq 'select' # select(F) doesn't have a proto
	 )
	 && $kid->name eq "rv2gv"
	 && !($kid->private & B::OPpLVAL_INTRO)
    ) {
	$first = $self->rv2gv_or_string($kid->first, $op);
    }
    else {
	$first = $self->deparse($kid, 6, $op);
    }
    if ($name eq "chmod" && $first->{text} =~ /^\d+$/) {
	my $transform_fn = sub {sprintf("%#o", $self->info2str(shift))};
	$first = $self->info_from_template("chmod octal", undef,
					   "%F", [[0, $transform_fn]],
					   [$first], {'relink_children' => [0]});
	push @new_nodes, $first;
    }

    # FIXME: fold this into a template
    $first->{text} = "+" + $first->{text}
	if not $parens and not $nollafr and substr($first->{text}, 0, 1) eq "(";

    push @exprs, $first;
    $kid = $kid->sibling;
    if (defined $proto && $proto =~ /^\*\*/ && $kid->name eq "rv2gv"
	&& !($kid->private & B::OPpLVAL_INTRO)) {
	$first = $self->rv2gv_or_string($kid->first, $op);
	push @exprs, $first;
	$kid = $kid->sibling;
    }
    for ( ; !null($kid); $kid = $kid->sibling) {
	my $expr = $self->deparse($kid, 6, $op);
	push @exprs, $expr;
    }

    if ($name eq "reverse" && ($op->private & B::OPpREVERSE_INPLACE)) {
	my $texts =  [$exprs[0->{text}], '=',
		      $fullname . ($parens ? "($exprs[0]->{text})" : " $exprs[0]->{text}")];
	return info_from_list($op, $self, $texts, ' ', 'listop_reverse', {});
    }

    my $opts = {};
    my $type;
    my $fmt;

    if ($name =~ /^(system|exec)$/
	&& ($op->flags & B::OPf_STACKED)
	&& @exprs > 1)
    {
	# handle the "system(prog a1, a2, ...)" form
	# where there is no ', ' between the first two arguments.
	if ($parens && $nollafr) {
	    $fmt = "($fullname %c %C)";
	    $type = "listop ($fullname)";
	} elsif ($parens) {
	    $fmt = "$fullname(%c %C)";
	    $type = "listop $fullname()";
	} else {
	    $fmt = "$fullname %c %C";
	    $type = "listop $fullname";
	}
	return $self->info_from_template($type, $op, $fmt,
					 [0, [1, $#exprs, ', ']], \@exprs);

    }

    $fmt = "%c %C";
    if ($parens && $nollafr) {
	$fmt = "($fullname %C)";
	$type = "listop ($fullname)";
    } elsif ($parens) {
	$fmt = "$fullname(%C)";
	$type = "listop $fullname()";
    } else {
	$fmt = "$fullname %C";
	$type = "listop $fullname";
    }
    return $self->info_from_template($type, $op, $fmt,
				     [[0, $#exprs, ', ']], \@exprs,
				     {'synthesized_nodes' => \@new_nodes,
				      'other_ops' => \@skipped_ops});
}

sub loop_common
{
    my $self = shift;
    my($op, $cx, $init) = @_;
    my $enter = $op->first;
    my $kid = $enter->sibling;

    my @skipped_ops = ($enter);
    local(@$self{qw'curstash warnings hints hinthash'})
		= @$self{qw'curstash warnings hints hinthash'};

    my ($body, @body);
    my @nodes = ();
    my ($bare, $cond_info) = (0, undef);
    my $fmt = '';
    my $var_fmt;
    my @args_spec = ();
    my $opts = {};
    my $type = 'loop';

    if ($kid->name eq "lineseq") {
	# bare or infinite loop
	$type .= ' while (1)';

	if ($kid->last->name eq "unstack") { # infinite
	    $fmt .= 'while (1)';
	} else {
	    $bare = 1;
	}
	$body = $kid;
    } elsif ($enter->name eq "enteriter") {
	# foreach
	$type .= ' foreach';

	my $ary = $enter->first->sibling; # first was pushmark
	push @skipped_ops, $enter->first, $ary->first->sibling;
	my ($ary_fmt, $var_info);
	my $var = $ary->sibling;
	if (null $var) {
	    if (($enter->flags & B::OPf_SPECIAL) && ($] < 5.009)) {
		# thread special var, under 5005threads
		$var_fmt = $self->pp_threadsv($enter, 1);
	    } else { # regular my() variable
		$var_info = $self->pp_padsv($enter, 1, 1);
		push @nodes, $var_info;
		$var_fmt = '%c';
		push @args_spec, $#nodes;
	    }
	} elsif ($var->name eq "rv2gv") {
	    $var_info = $self->pp_rv2sv($var, 1);
	    push @nodes, $var_info;
	    if ($enter->private & B::OPpOUR_INTRO) {
		# "our" declarations don't have package names
		my $transform_fn = sub {$_[0] =~ s/^(.).*::/$1/};
		$var_fmt = "our %F";
		push @args_spec, [$#nodes, $transform_fn];
	    } else {
		$var_fmt = '%c';
		push @args_spec, $#nodes;
	    }
	} elsif ($var->name eq "gv") {
	    $var_info = $self->deparse($var, 1, $op);
	    push @nodes, $var_info;
	    $var_fmt = '$%c';
	    push @args_spec, $#nodes;
	}

	if ($ary->name eq 'null' and $enter->private & B::OPpITER_REVERSED) {
	    # "reverse" was optimised away
	    push @nodes, listop($self, $ary->first->sibling, 1, 'reverse');
	    $ary_fmt = "%c";
	    push @args_spec, $#nodes;
	} elsif ($enter->flags & B::OPf_STACKED
		 and not null $ary->first->sibling->sibling) {
	    push @args_spec, scalar(@nodes), scalar(@nodes+1);
	    push @nodes, ($self->deparse($ary->first->sibling, 9, $op),
			 $self->deparse($ary->first->sibling->sibling, 9, $op));
	    $ary_fmt = '(%c .. %c)';

	} else {
	    push @nodes, $self->deparse($ary, 1, $op);
	    $ary_fmt = "%c";
	    push @args_spec, $#nodes;
	}

	# skip OP_AND and OP_ITER
	push @skipped_ops, $kid->first, $kid->first->first;
	$body = $kid->first->first->sibling;

	if (!B::Deparse::is_state $body->first
	    and $body->first->name !~ /^(?:stub|leave|scope)$/) {
	    # FIXME:
	   #  Carp::confess("var ne \$_") unless join('', @var_text) eq '$_';
	    push @skipped_ops, $body->first;
	    $body = $body->first;
	    my $body_info = $self->deparse($body, 2, $op);
	    push @nodes, $body_info;
	    return $self->info_from_template("foreach", $op,
					     "$var_fmt foreach ($ary_fmt)",
					     \@args_spec, \@nodes,
					     {other_ops => \@skipped_ops});
	}
	$fmt = "foreach $var_fmt $ary_fmt";
    } elsif ($kid->name eq "null") {
	# while/until

	$kid = $kid->first;
	my $name = {"and" => "while", "or" => "until"}->{$kid->name};
	$type .= " $name";
	$cond_info = $self->deparse($kid->first, 1, $op);
	$fmt = "$name (%c) ";
	push @nodes, $cond_info;
	$body = $kid->first->sibling;
	@args_spec = (0);
    } elsif ($kid->name eq "stub") {
	# bare and empty
	return info_from_text($op, $self, '{;}', 'empty loop', {});
    }

    # If there isn't a continue block, then the next pointer for the loop
    # will point to the unstack, which is kid's last child, except
    # in a bare loop, when it will point to the leaveloop. When neither of
    # these conditions hold, then the second-to-last child is the continue
    # block (or the last in a bare loop).
    my $cont_start = $enter->nextop;
    my ($cont, @cont_text, $body_info);
    my @cont = ();
    if ($$cont_start != $$op && ${$cont_start} != ${$body->last}) {
	$type .= ' continue';

	if ($bare) {
	    $cont = $body->last;
	} else {
	    $cont = $body->first;
	    while (!null($cont->sibling->sibling)) {
		$cont = $cont->sibling;
	    }
	}
	my $state = $body->first;
	my $cuddle = " ";
	my @states;
	for (; $$state != $$cont; $state = $state->sibling) {
	    push @states, $state;
	}
	$body_info = $self->lineseq(undef, 0, @states);
	if (defined $cond_info
	    and not B::Deparse::is_scope($cont)
	    and $self->{'expand'} < 3) {
	    my $cont_info = $self->deparse($cont, 1, $op);
	    my $init = defined($init) ? $init : ' ';
	    @nodes = ($init, $cond_info, $cont_info);
	    # @nodes_text = ('for', '(', "$init_text;", $cont_info->{text}, ')');
	    $fmt = 'for (%c; %c; %c) ';
	    @args_spec = (0, 1, 2);
	    $opts->{'omit_next_semicolon'} = 1;
	} else {
	    my $cont_info = $self->deparse($cont, 0, $op);
	    @nodes =  ($init, $cont_info);
	    @args_spec = (0, 1);
	    $opts->{'omit_next_semicolon'} = 1;
	    @cont_text = ($cuddle, 'continue', "{\n\t",
			  $cont_info->{text} , "\n\b}");
	}
    } else {
	return info_from_text($op, $self, '', 'loop_no_body', {})
	    if !defined $body;
	if (defined $init) {
	    @nodes = ($init, $cond_info);
	    $fmt = 'for (%c; %c;) ';
	    @args_spec = (0, 1);
	}
	$opts->{'omit_next_semicolon'} = 1;
	$body_info = $self->deparse($body, 0, $op);
    }

    # (my $body_text = $body_info->{text}) =~ s/;?$/;\n/;
    # my @texts = (@nodes_text, "{\n\t", $body_text, "\b}", @cont_text);

    push @nodes, $body_info;
    push @args_spec, $#nodes;
    $fmt .= " {\n%+%c%-\n}";
    if (@cont_text) {
	push @nodes, @cont_text;
	push @args_spec, $#nodes;
	$type .= ' cont';
	$fmt .= '%c';
    }
    return $self->info_from_template($type, $op, $fmt, \@args_spec, \@nodes, $opts)
}

# loop expressions
sub loopex
{
    my ($self, $op, $cx, $name) = @_;
    my $opts = {maybe_parens => [$self, $cx, 7]};
    my ($type, $body);
    if (B::class($op) eq "PVOP") {
	return info_from_list($op, $self, [$name, $op->pv], ' ', 'loopex_pvop', {});
    } elsif (B::class($op) eq "OP") {
	# no-op
	$type = 'loopex_op';
	return info_from_text($op, $self, $name, 'loopex_op', $opts);
    } elsif (B::class($op) eq "UNOP") {
	(my $kid_info = $self->deparse($op->first, 7, $op)) =~ s/^\cS//;
	$opts->{body} = [$kid_info];
	return info_from_list($op, $self, [$name, $op->pv], ' ', 'loopex_unop', $opts);
    } else {
	return info_from_text($op, $self, $name, 'loopex', $opts);
    }
    Carp::confess("unhandled condition in lopex");
}

sub indirop
{
    my($self, $op, $cx, $name) = @_;
    my($expr, @exprs);
    my $firstkid = my $kid = $op->first->sibling;
    my $indir_info;
    my @body = ();
    my $type = $name;
    my $first_op = $op->first;
    my @skipped_ops = ($first_op);
    my @indir = ();

    if ($op->flags & OPf_STACKED) {
	push @skipped_ops, $kid;
	my $indir_op = $kid->first; # skip rv2gv
	if (is_scope($indir_op)) {
	    $indir_info = $self->deparse($indir_op, 0, $op);
	    @indir = $indir_info->{text} eq '' ?
		("{", ';', "}") : ("{", $self->info2str($indir_info), "}");

	} elsif ($indir_op->name eq "const" && $indir_op->private & OPpCONST_BARE) {
	    @indir = ($self->const_sv($indir_op)->PV);
	} else {
	    $indir_info = $self->deparse($indir_op, 24, $op);
	    @indir = exists $indir_info->{texts} ?
		@{$indir_info->{texts}} : ();
	}
	push @body, $indir_info if $indir_info;
	$kid = $kid->sibling;
    }
    if ($name eq "sort" && $op->private & (OPpSORT_NUMERIC | OPpSORT_INTEGER)) {
	$type = 'sort_num_int';
	@indir = ($op->private & OPpSORT_DESCEND) ?
	    ('{', '$b', ' <=> ', '$a', '}' ) : ('{', '$a', ' <=> ', '$b', '}' );
    }
    elsif ($name eq "sort" && $op->private & OPpSORT_DESCEND) {
	$type = 'sort_descend';
	@indir =  ('{', '$b', ' cmp ', '$a', '}' );
    }

    for (; !null($kid); $kid = $kid->sibling) {
	my $cx = (!scalar(@indir) &&
		  $kid == $firstkid && $name eq "sort" &&
		  $firstkid->name eq "entersub")
	    ? 16 : 6;
	$expr = $self->deparse($kid, $cx, $op);
	push @exprs, $expr;
    }

    push @body, @exprs;
    my $opts = {
	body => \@body,
	other_ops => \@skipped_ops
    };

    my $name2;
    if ($name eq "sort" && $op->private & OPpSORT_REVERSE) {
	$type = 'sort_reverse';
	$name2 = $self->keyword('reverse') . ' ' . $self->keyword('sort');
    }  else {
	$name2 = $self->keyword($name);
    }
    my $indir = scalar @indir ? (join('', @indir) . ' ') : '';
    if ($name eq "sort" && ($op->private & OPpSORT_INPLACE)) {
	my @texts = ($exprs[0], '=', $name2, $indir, $exprs[0]);
	return info_from_list $op, $self, \@texts, '', 'sort_inplace', $opts;
    }

    my @texts;
    my $args = $indir . join(', ', map($_->{text},  @exprs));
    if ($indir ne "" && $name eq "sort") {
	# We don't want to say "sort(f 1, 2, 3)", since perl -w will
	# give bareword warnings in that case. Therefore if context
	# requires, we'll put parens around the outside "(sort f 1, 2,
	# 3)". Unfortunately, we'll currently think the parens are
	# necessary more often that they really are, because we don't
	# distinguish which side of an assignment we're on.
	if ($cx >= 5) {
	    @texts = ('(', $name2, $args, ')');
	} else {
	    @texts = ($name2, $args);
	}
	$type='sort1';
    } elsif (!$indir && $name eq "sort"
	     && !null($op->first->sibling)
	     && $op->first->sibling->name eq 'entersub' ) {
	# We cannot say sort foo(bar), as foo will be interpreted as a
	# comparison routine.  We have to say sort(...) in that case.
	@texts = ($name2, '(', $args, ')');
	$type='sort2';
    } else {
	# indir
	if (length $args) {
	    $type='indirop';
	    @texts = ($self->maybe_parens_func($name2, $args, $cx, 5))
	} else {
	    $type='indirop_noargs';
	    @texts = ($name2);
	    push(@texts, '()') if (7 < $cx);
	}
    }
    return info_from_list($first_op, $self, \@texts, '', "$type", $opts);
}

sub mapop
{
    my($self, $op, $cx, $name) = @_;
    my $kid = $op->first; # this is the (map|grep)start

    my @skipped_ops = ($kid, $kid->first);
    $kid = $kid->first->sibling; # skip a pushmark

    my $code = $kid->first; # skip a null

    my $code_info;

    my @block_texts = ();
    my @exprs_texts = ();
    if (is_scope $code) {
	$code_info = $self->deparse($code, 0, $op);
	(my $text = $code_info->{text})=~ s/^\n//;  # remove first \n in block.
	@block_texts = ('{', $text, '}');
    } else {
	$code_info = $self->deparse($code, 24, $op);
	@exprs_texts = ($code_info->{text});
    }
    my @body = ($code_info);

    push @skipped_ops, $kid;
    $kid = $kid->sibling;
    my($expr, @exprs);
    for (; !null($kid); $kid = $kid->sibling) {
	$expr = $self->deparse($kid, 6, $op);
	push @exprs, $expr if defined $expr;
    }
    push @body, @exprs;
    push @exprs_texts, map $_->{text}, @exprs;
    my $opts = {
	body => \@body,
	other_ops => \@skipped_ops,
    };
    my $params = join(', ', @exprs_texts);
    $params = join(" ", @block_texts) . ' ' . $params if @block_texts;
    my @texts = $self->maybe_parens_func($name, $params, $cx, 5);
    return info_from_list $op, $self, \@texts, '', 'mapop', $opts;
}

# osmic acid -- see osmium tetroxide

my %matchwords;
map($matchwords{join "", sort split //, $_} = $_, 'cig', 'cog', 'cos', 'cogs',
    'cox', 'go', 'is', 'ism', 'iso', 'mig', 'mix', 'osmic', 'ox', 'sic',
    'sig', 'six', 'smog', 'so', 'soc', 'sog', 'xi');

sub matchop
{
    my($self, $op, $cx, $name, $delim) = @_;
    my $kid = $op->first;
    my $info = {};
    my @body = ();
    my ($binop, $var, $re_str) = ("", "", "");
    my $re;
    if ($op->flags & B::OPf_STACKED) {
	$binop = 1;
	$var = $self->deparse($kid, 20, $op);
	push @body, $var;
	$kid = $kid->sibling;
    }
    my $quote = 1;
    my $pmflags = $op->pmflags;
    my $extended = ($pmflags & B::PMf_EXTENDED);
    my $rhs_bound_to_defsv;
    if (B::Deparse::null $kid) {
	my $unbacked = B::Deparse::re_unback($op->precomp);
	if ($extended) {
	    $re_str = B::Deparse::re_uninterp_extended(B::Deparse::escape_extended_re($unbacked));
	} else {
	    $re_str = B::Deparse::re_uninterp(B::Deparse::escape_str(B::Deparse::re_unback($op->precomp)));
	}
    } elsif ($kid->name ne 'regcomp') {
	carp("found ".$kid->name." where regcomp expected");
    } else {
	($re, $quote) = $self->regcomp($kid, 21, $extended);
	push @body, $re;
	$re_str = $re->{text};
	my $matchop = $kid->first;
	if ($matchop->name eq 'regcrest') {
	    $matchop = $matchop->first;
	}
	if ($matchop->name =~ /^(?:match|transr?|subst)\z/
	   && $matchop->flags & B::OPf_SPECIAL) {
	    $rhs_bound_to_defsv = 1;
	}
    }
    my $flags = '';
    $flags .= "c" if $pmflags & B::PMf_CONTINUE;
    $flags .= $self->re_flags($op);
    $flags = join '', sort split //, $flags;
    $flags = $matchwords{$flags} if $matchwords{$flags};

    if ($pmflags & B::PMf_ONCE) { # only one kind of delimiter works here
	$re_str =~ s/\?/\\?/g;
	$re_str = "?$re_str?";
    } elsif ($quote) {
	my $re = $self->single_delim($kid, $name, $delim, $re_str);
	push @body, $re;
	$re_str = $re->{text};
    }
    my $opts = {body => \@body};
    my @texts;
    $re_str .= $flags if $quote;
    my $type;
    if ($binop) {
	if ($rhs_bound_to_defsv) {
	    @texts = ($var->{text}, ' =~ ', "(", '$_', ' =~ ', $re_str, ')');
	} else {
	    @texts = ($var->{text}, ' =~ ', $re_str);
	}
	$opts->{maybe_parens} = [$self, $cx, 20];
	$type = 'matchop_binop';
    } else {
	@texts = ($re_str);
	$type = 'matchop_unnop';
    }
    return info_from_list($op, $self, \@texts, '', $type, $opts);
}

sub unop
{
    my($self, $op, $cx, $name, $nollafr) = @_;
    my $kid;
    if ($op->flags & B::OPf_KIDS) {
	$kid = $op->first;
 	if (not $name) {
 	    # this deals with 'boolkeys' right now
 	    return $self->deparse($kid, $cx, $op);
 	}
	my $builtinname = $name;
	$builtinname =~ /^CORE::/ or $builtinname = "CORE::$name";
	if (defined prototype($builtinname)
	   && $builtinname ne 'CORE::readline'
	   && prototype($builtinname) =~ /^;?\*/
	   && $kid->name eq "rv2gv") {
	    $kid = $kid->first;
	}

	if ($nollafr) {
	    $kid = $self->deparse($kid, 16, $op);
	    ($kid->{text}) =~ s/^\cS//;
	    my $opts = {
		body => [$kid],
		maybe_parens => [$self, $cx, 16],
	    };
	    return info_from_list($op, $self, [($self->keyword($name), $kid->{text})],
				  ' ', 'unary operator noallafr', $opts);
	}
	return $self->maybe_parens_unop($name, $kid, $cx, $op);
    } else {
	my $opts = {maybe_parens => [$self, $cx, 16]};
	my @texts = ($self->keyword($name));
	push @texts, '()' if $op->flags & B::OPf_SPECIAL;
	return info_from_list($op, $self, \@texts, '', 'unary operator', $opts);
    }
}

sub POSTFIX () { 1 }

# This is the category of symbolic prefix and postfix unary operators,
# e.g $x++, -r, +$x.
sub pfixop
{
    my $self = shift;
    my($op, $cx, $operator, $prec, $flags) = (@_, 0);
    my $operand = $self->deparse($op->first, $prec, $op);
    my ($type, $fmt);
    my @nodes;
    if ($flags & POSTFIX) {
	@nodes = ($operand, $operator);
	$type = "prefix $operator";
	$fmt = "%c%c";
    } elsif ($operator eq '-' && $operand->{text} =~ /^[a-zA-Z](?!\w)/) {
	# Add () around operator to disambiguate with filetest operator
	@nodes = ($operator, $operand);
	$type = "prefix non-filetest $operator";
	$fmt = "%c(%c)";
    } else {
	@nodes = ($operator, $operand);
	$type = "postfix $operator";
	$fmt = "%c%c";
    }

    return $self->info_from_template($type, $op, $fmt, [0, 1],
				     \@nodes,
				     {maybe_parens => [$self, $cx, $prec]}) ;
}

# Demo code
unless(caller) {
    ;
}

1;
