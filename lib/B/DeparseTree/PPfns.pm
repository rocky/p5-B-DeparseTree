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
use B;
use B::DeparseTree::Common;

our($VERSION, @EXPORT, @ISA);
$VERSION = '3.1.1';
@ISA = qw(Exporter);
@EXPORT = qw(
    binop
    dq_unop
    givwhen
    listop
    loop_common
    matchop
    unop
    );

sub binop
{
    my ($self, $op, $cx, $opname, $prec, $flags) = (@_, 0);
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
    my @texts = ($lhs, "$opname$eq", $rhs);
    return info_from_list($op, $self, \@texts, ' ', "binary operator $opname$eq",
			  {maybe_parens => [$self, $cx, $prec]});
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
	return info_from_text($op, $self, $text, "listop $name", {});
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

	if (!is_state $body->first and $body->first->name !~ /^(?:stub|leave|scope)$/) {
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
	if (defined $cond_info and not is_scope($cont) and $self->{'expand'} < 3) {
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

# Demo code
unless(caller) {
    ;
}

1;
