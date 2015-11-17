# Copyright (c) 2015 Rocky Bernstein
use strict; use warnings;

use rlib '../..';
package B::DeparseTree::Common;

use B::DeparseTree::Node;

use B qw(class opnumber OPpLVAL_INTRO OPf_SPECIAL OPf_KIDS SVf_ROK
         OPpTARGET_MY svref_2object perlstring);
use Carp;

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.1.0';
@ISA = qw(Exporter);
@EXPORT = qw(maybe_parens info_from_list info_from_text null new WARN_MASK
            indent_list indent indent_info parens_test print_protos
            style_opts scopeop declare_hints hint_pragmas
            is_miniwhile is_lexical_subs %strict_bits
            %rev_feature declare_hinthash declare_warnings %ignored_hints
            _features_from_bundle ambiant_pragmas maybe_qualify
            %globalnames gv_name is_scope logop maybe_targmy is_state
            POSTFIX baseop mapop pfixop
            deparse_root
            );

our %strict_bits = do {
    local $^H;
    map +($_ => strict::bits($_)), qw/refs subs vars/
};

BEGIN {
    # List version-specific constants here.
    # Easiest way to keep this code portable between version looks to
    # be to fake up a dummy constant that will never actually be true.
    foreach (qw(OPpSORT_INPLACE OPpSORT_DESCEND OPpITER_REVERSED OPpCONST_NOVER
		OPpPAD_STATE PMf_SKIPWHITE RXf_SKIPWHITE
		RXf_PMf_CHARSET RXf_PMf_KEEPCOPY
		CVf_LOCKED OPpREVERSE_INPLACE OPpSUBSTR_REPL_FIRST
		PMf_NONDESTRUCT OPpCONST_ARYBASE OPpEVAL_BYTES)) {
	eval { import B $_ };
	no strict 'refs';
	*{$_} = sub () {0} unless *{$_}{CODE};
    }
}

BEGIN { for (qw[ pushmark ]) {
    eval "sub OP_\U$_ () { " . opnumber($_) . "}"
}}

# $root should be the op which represents the root of whatever
# we're sequencing here. If it's undefined, then we don't append
# any subroutine declarations to the deparsed ops, otherwise we
# append appropriate declarations.
sub lineseq {
    my($self, $root, $cx, @ops) = @_;

    my $out_cop = $self->{'curcop'};
    my $out_seq = defined($out_cop) ? $out_cop->cop_seq : undef;
    my $limit_seq;
    if (defined $root) {
	$limit_seq = $out_seq;
	my $nseq;
	$nseq = $self->find_scope_st($root->sibling) if ${$root->sibling};
	$limit_seq = $nseq if !defined($limit_seq)
			   or defined($nseq) && $nseq < $limit_seq;
    }
    $limit_seq = $self->{'limit_seq'}
	if defined($self->{'limit_seq'})
	&& (!defined($limit_seq) || $self->{'limit_seq'} < $limit_seq);
    local $self->{'limit_seq'} = $limit_seq;

    my $fn = sub {
	my ($exprs, $i, $info, $parent) = @_;
	my $text = $info->{text};
	my $op = $ops[$i];
	$text =~ s/\f//;
	$text =~ s/\n$//;
	$text =~ s/;\n?\z//;
	$info->{type} = $op->name;
	$info->{op} = $op;
	if ($parent) {
	    Carp::confess("nonref parent, op: $op->name") if !ref($parent);
	    $info->{parent} = $$parent ;
	}
	if ($info->{body}) {
	    foreach my $bod (@{$info->{body}{body}}) {
		if (ref($bod)) {
		    $bod->{parent} = $$op;
		}
	    }
	}
	$self->{optree}{$$op} = $info;
	$info->{text} = $text;
	push @$exprs, $info;
    };
    return $self->walk_lineseq($root, \@ops, $fn);
}

sub null
{
    my $op = shift;
    return class($op) eq "NULL";
}

# This is a special case of scopeop and lineseq, for the case of the
# main_root.
sub deparse_root {
    my $self = shift;
    my($op) = @_;
    local(@$self{qw'curstash warnings hints hinthash'})
      = @$self{qw'curstash warnings hints hinthash'};
    my @ops;
    return if null $op->first; # Can happen, e.g., for Bytecode without -k
    for (my $kid = $op->first->sibling; !null($kid); $kid = $kid->sibling) {
	push @ops, $kid;
    }
    my $fn = sub {
	my ($exprs, $i, $info, $parent) = @_;
	my $text = $info->{text};
	my $op = $ops[$i];
	$text =~ s/\f//;
	$text =~ s/\n$//;
	$text =~ s/;\n?\z//;
	$text =~ s/^\((.+)\)$/$1/;
	$info->{type} = $op->name;
	$info->{op} = $op;
	$self->{optree}{$$op} = $info;
	$info->{text} = $text;
	$info->{parent} = $$parent if $parent;
	push @$exprs, $info;
    };
    return $self->walk_lineseq($op, \@ops, $fn);
}

sub is_state {
    my $name = $_[0]->name;
    return $name eq "nextstate" || $name eq "dbstate" || $name eq "setstate";
}

# Check if the op and its sibling are the initialization and the rest of a
# for (..;..;..) { ... } loop
sub is_for_loop {
    my $op = shift;
    # This OP might be almost anything, though it won't be a
    # nextstate. (It's the initialization, so in the canonical case it
    # will be an sassign.) The sibling is (old style) a lineseq whose
    # first child is a nextstate and whose second is a leaveloop, or
    # (new style) an unstack whose sibling is a leaveloop.
    my $lseq = $op->sibling;
    return 0 unless !is_state($op) and !null($lseq);
    if ($lseq->name eq "lineseq") {
	if ($lseq->first && !null($lseq->first) && is_state($lseq->first)
	    && (my $sib = $lseq->first->sibling)) {
	    return (!null($sib) && $sib->name eq "leaveloop");
	}
    } elsif ($lseq->name eq "unstack" && ($lseq->flags & OPf_SPECIAL)) {
	my $sib = $lseq->sibling;
	return $sib && !null($sib) && $sib->name eq "leaveloop";
    }
    return 0;
}

sub walk_lineseq {
    my ($self, $op, $kids, $callback) = @_;
    my @kids = @$kids;
    my @body = ();
    my $expr;
    my $parent = $op;
    for (my $i = 0; $i < @kids; $i++) {
	if (is_state $kids[$i]) {
	    $expr = ($self->deparse($kids[$i], 0, $parent));
	    $callback->(\@body, $i, $expr);
	    $i++;
	    if ($i > $#kids) {
		last;
	    }
	}
	if (is_for_loop($kids[$i])) {
	    my $loop_expr = $self->for_loop($kids[$i], 0);
	    $loop_expr =~ s/\cK//;
	    $callback->(\@body,
			$i += $kids[$i]->sibling->name eq "unstack" ? 2 : 1,
			$loop_expr);
	    next;
	}
	$expr = $self->deparse($kids[$i], (@kids != 1)/2, $parent);
	$callback->(\@body, $i, $expr, $parent);
    }

    # Add semicolons between statements. Don't add them to blank lines,
    # or to comment lines, which we
    # assume will always be the last line in the text and start after a \n.
    my @texts = map { $_->{text} =~ /(?:\n#)|^\s*$/ ?
			  $_->{text} : $_->{text} . ';' } @body;
    my $info = {
	type => 'lineseq',
	op => $op,
	body => \@body,
	texts => \@texts,
	text => join("\n", @texts)
    };
    $self->{optree}{$$op} = $info if $op;
    return $info;
}

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->{'curcop'} = undef;
    $self->{'curstash'} = "main";
    $self->{'ex_const'} = "'???'";
    $self->{'expand'} = 0;
    $self->{'files'} = {};
    $self->{'indent_size'} = 4;
    $self->{'opaddr'} = 0;
    $self->{'linenums'} = 0;
    $self->{'parens'} = 0;
    $self->{'subs_todo'} = [];
    $self->{'unquote'} = 0;
    $self->{'use_dumper'} = 0;
    $self->{'use_tabs'} = 0;

    $self->{'ambient_arybase'} = 0;
    $self->{'ambient_warnings'} = undef; # Assume no lexical warnings
    $self->{'ambient_hints'} = 0;
    $self->{'ambient_hinthash'} = undef;

    # Given an opcode address, get the accumulated OP tree
    # OP for that.
    $self->{optree} = {};

    $self->init();

    while (my $arg = shift @_) {
	if ($arg eq "-d") {
	    $self->{'use_dumper'} = 1;
	    require Data::Dumper;
	} elsif ($arg =~ /^-f(.*)/) {
	    $self->{'files'}{$1} = 1;
	} elsif ($arg eq "-l") {
	    $self->{'linenums'} = 1;
	} elsif ($arg eq "-a") {
	    $self->{'linenums'} = 1;
	    $self->{'opaddr'} = 1;
	} elsif ($arg eq "-p") {
	    $self->{'parens'} = 1;
	} elsif ($arg eq "-P") {
	    $self->{'noproto'} = 1;
	} elsif ($arg eq "-q") {
	    $self->{'unquote'} = 1;
	} elsif (substr($arg, 0, 2) eq "-s") {
	    $self->style_opts(substr $arg, 2);
	} elsif ($arg =~ /^-x(\d)$/) {
	    $self->{'expand'} = $1;
	}
    }
    return $self;
}

{
    # Mask out the bits that L<warnings::register> uses
    my $WARN_MASK;
    BEGIN {
	$WARN_MASK = $warnings::Bits{all} | $warnings::DeadBits{all};
    }
    sub WARN_MASK () {
	return $WARN_MASK;
    }
}

# Initialize the contextual information, either from
# defaults provided with the ambient_pragmas method,
# or from perl's own defaults otherwise.
sub init($)
{
    my $self = shift;

    $self->{'arybase'}  = $self->{'ambient_arybase'};
    $self->{'warnings'} = defined ($self->{'ambient_warnings'})
				? $self->{'ambient_warnings'} & WARN_MASK
				: undef;
    $self->{'hints'}    = $self->{'ambient_hints'};
    $self->{'hinthash'} = $self->{'ambient_hinthash'};

    # also a convenient place to clear out subs_declared
    delete $self->{'subs_declared'};
}

sub coderef2text {
    my $self = shift;
    my $func = shift;
    croak "Usage: ->coderef2text(CODEREF)" unless UNIVERSAL::isa($func, "CODE");

    $self->init();
    my $info = $self->coderef2list($func);
    return $self->indent($info->{text});
}

sub coderef2list {
    my ($self, $coderef) = @_;
    croak "Usage: ->coderef2list(CODEREF)" unless UNIVERSAL::isa($coderef, "CODE");
    $self->init();
    return $self->deparse_sub(svref_2object($coderef));
}

sub print_protos($)
{
    my $self = shift;
    my $ar;
    my @ret;
    foreach $ar (@{$self->{'protos_todo'}}) {
	my $proto = (defined $ar->[1] ? " (". $ar->[1] . ")" : "");
	push @ret, "sub " . $ar->[0] .  "$proto;\n";
    }
    delete $self->{'protos_todo'};
    return @ret;
}

sub style_opts($$)
{
    my ($self, $opts) = @_;
    my $opt;
    while (length($opt = substr($opts, 0, 1))) {
	if ($opt eq "T") {
	    $self->{'use_tabs'} = 1;
	    $opts = substr($opts, 1);
	} elsif ($opt eq "v") {
	    $opts =~ s/^v([^.]*)(.|$)//;
	    $self->{'ex_const'} = $1;
	} else {
	    $opts = substr($opts, 1);
	}
    }
}

sub maybe_targmy
{
    my($self, $op, $cx, $func, @args) = @_;
    if ($op->private & OPpTARGET_MY) {
	my $var = $self->padname($op->targ);
	my $val = $func->($self, $op, 7, @args);
	return info_from_list([$var, '=', $val->{text}],
			      ' ', 'maybe_targmy',
			      {maybe_parens => [$self, $cx, 7]});
    } else {
	return $func->($self, $op, $cx, @args);
    }
}

# FIXME: remove
sub parens_test($$) {
    my ($cx, $prec) = @_;
    return ($prec < $cx
	    # unary ops nest just fine
	    or $prec == $cx and $cx != 4 and $cx != 16 and $cx != 21)
}

# Possibly add () around $text depending on precidence $prec and
# context $cx. We return a string.
sub maybe_parens($$$$)
{
    my($self, $info, $cx, $prec) = @_;
    if (parens_test($cx, $prec) or $self->{'parens'}) {
	$info->{text} = "($info->{text})";
	# In a unop, let parent reuse our parens; see maybe_parens_unop
	if ($cx == 16) {
	    $info->{text} = "\cS" . $info->{text};
	    $info->{parens} = 'reuse';
	}  else {
	    $info->{parens} = 'true';
	}
	return $info->{text};
    } else {
	$info->{parens} = '';
	return $info->{text};
    }
}

# FIXME: remove
# Possibly add () around $text depending on precidence $prec and
# context $cx. We return a string.
# Possibly add () around $text depending on precidence $prec and
# context $cx. We return a string.
sub maybe_parens_str($$$$)
{
    my($self, $text, $cx, $prec) = @_;
    if (parens_test($cx, $prec) or $self->{'parens'}) {
	$text = "($text)";
	# In a unop, let parent reuse our parens; see maybe_parens_unop
	$text = "\cS" . $text if $cx == 16;
	return $text;
    } else {
	return $text;
    }
}

# FIXME: remove
*info_from_text = \&B::DeparseTree::Node::from_str;
*info_from_list = \&B::DeparseTree::Node::from_list;

sub todo
{
    my $self = shift;
    my($cv, $is_form) = @_;
    return unless ($cv->FILE eq $0 || exists $self->{files}{$cv->FILE});
    my $seq;
    if ($cv->OUTSIDE_SEQ) {
	$seq = $cv->OUTSIDE_SEQ;
    } elsif (!null($cv->START) and is_state($cv->START)) {
	$seq = $cv->START->cop_seq;
    } else {
	$seq = 0;
    }
    push @{$self->{'subs_todo'}}, [$seq, $cv, $is_form];
    unless ($is_form || class($cv->STASH) eq 'SPECIAL') {
	$self->{'subs_deparsed'}{$cv->STASH->NAME."::".$cv->GV->NAME} = 1;
    }
}

# _pessimize_walk(): recursively walk the optree of a sub,
# possibly undoing optimisations along the way.

sub _pessimize_walk($$) {
    my ($self, $startop) = @_;

    return unless $$startop;
    my ($op, $prevop);
    for ($op = $startop; $$op; $prevop = $op, $op = $op->sibling) {
	my $ppname = $op->name;

	# pessimisations start here

	if ($ppname eq "padrange") {
	    # remove PADRANGE:
	    # the original optimisation either (1) changed this:
	    #    pushmark -> (various pad and list and null ops) -> the_rest
	    # or (2), for the = @_ case, changed this:
	    #    pushmark -> gv[_] -> rv2av -> (pad stuff)       -> the_rest
	    # into this:
	    #    padrange ----------------------------------------> the_rest
	    # so we just need to convert the padrange back into a
	    # pushmark, and in case (1), set its op_next to op_sibling,
	    # which is the head of the original chain of optimised-away
	    # pad ops, or for (2), set it to sibling->first, which is
	    # the original gv[_].

	    $B::overlay->{$$op} = {
		    type => OP_PUSHMARK,
		    name => 'pushmark',
		    private => ($op->private & OPpLVAL_INTRO),
		    next    => ($op->flags & OPf_SPECIAL)
				    ? $op->sibling->first
				    : $op->sibling,
	    };
	}

	# pessimizations end here

	if (class($op) eq 'PMOP'
	    && ref($op->pmreplroot)
	    && ${$op->pmreplroot}
	    && $op->pmreplroot->isa( 'B::OP' ))
	{
	    $self-> _pessimize_walk($op->pmreplroot);
	}

	if ($op->flags & OPf_KIDS) {
	    $self-> _pessimize_walk($op->first);
	}

    }
}


# _pessimise_walk_exe(): recursively walk the op_next chain of a sub,
# possibly undoing optimisations along the way.

sub _pessimize_walk_exe {
    my ($self, $startop, $visited) = @_;

    return unless $$startop;
    return if $visited->{$$startop};
    my ($op, $prevop);
    for ($op = $startop; $$op; $prevop = $op, $op = $op->next) {
	last if $visited->{$$op};
	$visited->{$$op} = 1;
	my $ppname = $op->name;
	if ($ppname =~
	    /^((and|d?or)(assign)?|(map|grep)while|range|cond_expr|once)$/
	    # entertry is also a logop, but its op_other invariably points
	    # into the same chain as the main execution path, so we skip it
	) {
	    $self->_pessimize_walk_exe($op->other, $visited);
	}
	elsif ($ppname eq "subst") {
	    $self->_pessimize_walk_exe($op->pmreplstart, $visited);
	}
	elsif ($ppname =~ /^(enter(loop|iter))$/) {
	    # redoop and nextop will already be covered by the main block
	    # of the loop
	    $self->_pessimize_walk_exe($op->lastop, $visited);
	}

	# pessimizations start here
    }
}

# Go through an optree and "remove" some optimisations by using an
# overlay to selectively modify or un-null some ops. Deparsing in the
# absence of those optimizations is then easier.
#
# Note that older optimizations are not removed, as Deparse was already
# written to recognise them before the pessimize/overlay system was added.

sub pessimize {
    my ($self, $root, $start) = @_;

    # walk tree in root-to-branch order
    $self->_pessimize_walk($root);

    my %visited;
    # walk tree in execution order
    $self->_pessimize_walk_exe($start, \%visited);
}


# This method is the inner loop, so try to keep it simple
sub deparse($$$$)
{
    my($self, $op, $cx, $parent) = @_;

    Carp::confess("Null op in deparse") if !defined($op)
					|| class($op) eq "NULL";
    my $meth = "pp_" . $op->name;
    # print "YYY $meth\n";
    my $info = $self->$meth($op, $cx);
    Carp::confess("nonref return for $meth deparse") if !ref($info);
    $info->{parent} = $$parent if $parent;
    $info->{cop} = $self->{'curcop'};
    my $got_op = $info->{op};
    if ($got_op) {
	if ($got_op != $op) {
	    # Do something here?
	    # printf("XX final op 0x%x is not requested 0x%x\n",
	    # 	   $$op, $$got_op);
	}
    } else {
	$info->{op} = $op;
    }
    $self->{optree}{$$op} = $info;
    if ($info->{other_ops}) {
	foreach my $other (@{$info->{other_ops}}) {
	    if (!ref $other) {
		Carp::confess "Invalid $other";
	    }
	    $self->{optree}{$$other} = $info;
	}
    }
    return $info;
}

sub indent_list($$)
{
    my ($self, $lines_ref) = @_;
    my $leader = "";
    my $level = 0;
    for my $line (@{$lines_ref}) {
	my $cmd = substr($line, 0, 1);
	if ($cmd eq "\t" or $cmd eq "\b") {
	    $level += ($cmd eq "\t" ? 1 : -1) * $self->{'indent_size'};
	    if ($self->{'use_tabs'}) {
		$leader = "\t" x ($level / 8) . " " x ($level % 8);
	    } else {
		$leader = " " x $level;
	    }
	    $line = substr($line, 1);
	}
	if (index($line, "\f") > 0) {
		$line =~ s/\f/\n/;
	}
	if (substr($line, 0, 1) eq "\f") {
	    $line = substr($line, 1); # no indent
	} else {
	    $line = $leader . $line;
	}
	$line =~ s/\cK;?//g;
    }

    my @lines = grep $_ !~ /^\s*$/, @$lines_ref;
    return join("\n", @lines)
}

sub indent($$)
{
    my ($self, $text) = @_;
    my @lines = split(/\n/, $text);
    return $self->indent_list(\@lines);
}

# Like indent, but takes an info object and removes leading
# parenthesis.
sub indent_info($$)
{
    my ($self, $info) = @_;
    my $text = $info->{text};
    if ($info->{maybe_parens} and
	substr($text, 0, 1) eq '(' and
	substr($text, -1) eq ')' ) {
	$text = substr($text, 1, length($text) -2)
    }
    return $self->indent($text);
}

sub is_lexical_subs {
    my (@ops) = shift;
    for my $op (@ops) {
        return 0 if $op->name !~ /\A(?:introcv|clonecv)\z/;
    }
    return 1;
}

sub is_miniwhile { # check for one-line loop ('foo() while $y--')
    my $op = shift;
    return (!null($op) and null($op->sibling)
	    and $op->name eq "null" and class($op) eq "UNOP"
	    and (($op->first->name =~ /^(and|or)$/
		  and $op->first->first->sibling->name eq "lineseq")
		 or ($op->first->name eq "lineseq"
		     and not null $op->first->first->sibling
		     and $op->first->first->sibling->name eq "unstack")
		 ));
}

sub scopeop
{
    my($real_block, $self, $op, $cx) = @_;
    my $kid;
    my @kids;

    local(@$self{qw'curstash warnings hints hinthash'})
		= @$self{qw'curstash warnings hints hinthash'} if $real_block;
    if ($real_block) {
	$kid = $op->first->sibling; # skip enter
	if (is_miniwhile($kid)) {
	    my $top = $kid->first;
	    my $name = $top->name;
	    if ($name eq "and") {
		$name = "while";
	    } elsif ($name eq "or") {
		$name = "until";
	    } else { # no conditional -> while 1 or until 0
		my $body = [$self->deparse($top->first, 1, $top)];
		return info_from_list([$body->{text}, 'while', '1'],
				      ' ', 'while_1', {body =>$body});
	    }
	    my $cond = $top->first;
	    my $other_ops = [$cond->sibling];
	    my $body = $cond->sibling->first; # skip lineseq
	    my $cond_info = $self->deparse($cond, 1, $top);
	    my $body_info = $self->deparse($body, 1, $top);
	    my @texts = ($body_info->{text}, $name, $cond_info->{text});
	    my $text = join(' ', @texts);
	    return {
		type => 'scopeop_block',
		body => [$cond_info, $body_info],
		texts => \@texts,
		text => $text,
		other_ops => $other_ops,
	    };
	}
    } else {
	$kid = $op->first;
    }
    for (; !null($kid); $kid = $kid->sibling) {
	push @kids, $kid;
    }
    if ($cx > 0) {
	# inside an expression, (a do {} while for lineseq)
	my $body = $self->lineseq($op, 0, @kids);
	my @texts = ($body->{text});
	my $text;
	unless (is_lexical_subs(@kids)) {
	    @texts = ('do ', "{\n\t", @texts, "\n\b}");
	};
	return {
	    type => 'scope_expr',
	    text => join('', @texts),
	    texts => \@texts,
	};
    } else {
	my $ls = $self->lineseq($op, $cx, @kids);
	my $text = $ls->{text};
	return info_from_text($text, 'scopeop', {});
    }
}

sub hint_pragmas($) {
    my ($bits) = @_;
    my (@pragmas, @strict);
    push @pragmas, "integer" if $bits & 0x1;
    for (sort keys %strict_bits) {
	push @strict, "'$_'" if $bits & $strict_bits{$_};
    }
    if (@strict == keys %strict_bits) {
	push @pragmas, "strict";
    }
    elsif (@strict) {
	push @pragmas, "strict " . join ', ', @strict;
    }
    push @pragmas, "bytes" if $bits & 0x8;
    return @pragmas;
}

sub declare_hints($$)
{
    my ($from, $to) = @_;
    my $use = $to   & ~$from;
    my $no  = $from & ~$to;
    my $decls = "";
    for my $pragma (hint_pragmas($use)) {
	$decls .= "use $pragma;\n";
    }
    for my $pragma (hint_pragmas($no)) {
        $decls .= "no $pragma;\n";
    }
    return $decls;
}

# Internal implementation hints that the core sets automatically, so don't need
# (or want) to be passed back to the user
my %ignored_hints = (
    'open<' => 1,
    'open>' => 1,
    ':'     => 1,
    'strict/refs' => 1,
    'strict/subs' => 1,
    'strict/vars' => 1,
);

my %rev_feature;

sub declare_hinthash {
    my ($from, $to, $indent, $hints) = @_;
    my $doing_features =
	($hints & $feature::hint_mask) == $feature::hint_mask;
    my @decls;
    my @features;
    my @unfeatures; # bugs?
    for my $key (sort keys %$to) {
	next if $ignored_hints{$key};
	my $is_feature = $key =~ /^feature_/ && $^V ge 5.15.6;
	next if $is_feature and not $doing_features;
	if (!exists $from->{$key} or $from->{$key} ne $to->{$key}) {
	    push(@features, $key), next if $is_feature;
	    push @decls,
		qq(\$^H{) . single_delim("q", "'", $key) . qq(} = )
	      . (
		   defined $to->{$key}
			? single_delim("q", "'", $to->{$key})
			: 'undef'
		)
	      . qq(;);
	}
    }
    for my $key (sort keys %$from) {
	next if $ignored_hints{$key};
	my $is_feature = $key =~ /^feature_/ && $^V ge 5.15.6;
	next if $is_feature and not $doing_features;
	if (!exists $to->{$key}) {
	    push(@unfeatures, $key), next if $is_feature;
	    push @decls, qq(delete \$^H{'$key'};);
	}
    }
    my @ret;
    if (@features || @unfeatures) {
	if (!%rev_feature) { %rev_feature = reverse %feature::feature }
    }
    if (@features) {
	push @ret, "use feature "
		 . join(", ", map "'$rev_feature{$_}'", @features) . ";\n";
    }
    if (@unfeatures) {
	push @ret, "no feature "
		 . join(", ", map "'$rev_feature{$_}'", @unfeatures)
		 . ";\n";
    }
    @decls and
	push @ret,
	     join("\n" . (" " x $indent), "BEGIN {", @decls) . "\n}\n";
    return @ret;
}

sub declare_warnings {
    my ($from, $to) = @_;
    if (($to & WARN_MASK) eq (warnings::bits("all") & WARN_MASK)) {
	return "use warnings;\n";
    }
    elsif (($to & WARN_MASK) eq ("\0"x length($to) & WARN_MASK)) {
	return "no warnings;\n";
    }
    return "BEGIN {\${^WARNING_BITS} = ".perlstring($to)."}\n";
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

# The BEGIN {} is used here because otherwise this code isn't executed
# when you run B::Deparse on itself.
my %globalnames;
BEGIN { map($globalnames{$_}++, "SIG", "STDIN", "STDOUT", "STDERR", "INC",
	    "ENV", "ARGV", "ARGVOUT", "_"); }

sub maybe_qualify {
    my ($self,$prefix,$name) = @_;
    my $v = ($prefix eq '$#' ? '@' : $prefix) . $name;
    return $name if !$prefix || $name =~ /::/;
    return $self->{'curstash'}.'::'. $name
	if
	    $name =~ /^(?!\d)\w/         # alphabetic
	 && $v    !~ /^\$[ab]\z/	 # not $a or $b
	 && !$globalnames{$name}         # not a global name
	 && $self->{hints} & $strict_bits{vars}  # strict vars
	 && !$self->lex_in_scope($v,1)   # no "our"
      or $self->lex_in_scope($v);        # conflicts with "my" variable
    return $name;
}

sub gv_name {
    my $self = shift;
    my $gv = shift;
    my $raw = shift;
    Carp::confess() unless ref($gv) eq "B::GV";
    my $cv = $gv->FLAGS & SVf_ROK ? $gv->RV : 0;
    my $stash = ($cv || $gv)->STASH->NAME;
    my $name = $raw
	? $cv ? $cv->NAME_HEK || $cv->GV->NAME : $gv->NAME
	: $cv
	    ? B::safename($cv->NAME_HEK || $cv->GV->NAME)
	    : $gv->SAFENAME;
    if ($stash eq 'main' && $name =~ /^::/) {
	$stash = '::';
    }
    elsif (($stash eq 'main'
	    && ($globalnames{$name} || $name =~ /^[^A-Za-z_:]/))
	or ($stash eq $self->{'curstash'} && !$globalnames{$name}
	    && ($stash eq 'main' || $name !~ /::/))
	  )
    {
	$stash = "";
    } else {
	$stash = $stash . "::";
    }
    if (!$raw and $name =~ /^(\^..|{)/) {
        $name = "{$name}";       # ${^WARNING_BITS}, etc and ${
    }
    return $stash . $name;
}

sub is_scope {
    my $op = shift;
    return $op->name eq "leave" || $op->name eq "scope"
      || $op->name eq "lineseq"
	|| ($op->name eq "null" && class($op) eq "UNOP"
	    && (is_scope($op->first) || $op->first->name eq "enter"));
}

# Logical ops, if/until, &&, and
# The one-line while/until is handled in pp_leave
sub logop
{
    my ($self, $op, $cx, $lowop, $lowprec, $highop,
	$highprec, $blockname) = @_;
    my $left = $op->first;
    my $right = $op->first->sibling;
    my ($lhs, $rhs, $texts, $text);
    my $type;
    my $sep;
    my $opts = {};
    if ($cx < 1 and is_scope($right) and $blockname
	and $self->{'expand'} < 7) {
	# if ($a) {$b}
	$lhs = $self->deparse($left, 1, $op);
	$rhs = $self->deparse($right, 0, $op);
	$sep = '';
	$texts = [$blockname, ' ', '(', $lhs->{text}, ')', ' ',
		  "{\n\t", $rhs->{text}, "\n\b}\cK"];
    } elsif ($cx < 1 and $blockname and not $self->{'parens'}
	     and $self->{'expand'} < 7) { # $b if $a
	$lhs = $self->deparse($left, 1, $op);
	$rhs = $self->deparse($right, 1, $op);
	$texts = [$rhs->{text}, $blockname, $lhs->{text}];
	$sep = ' ';
	$text = join(' ', @$texts);
    } elsif ($cx > $lowprec and $highop) {
	# $a && $b
	$lhs = $self->deparse_binop_left($op, $left, $highprec);
	$rhs = $self->deparse_binop_right($op, $right, $highprec);
	$texts = [$lhs->{text}, $highop, $rhs->{text}];
	$sep = ' ';
	$opts = {maybe_parens => [$self, $cx, $highprec]};
    } else {
	# $a and $b
	$lhs = $self->deparse_binop_left($op, $left, $lowprec);
	$rhs = $self->deparse_binop_right($op, $right, $lowprec);
	$texts = [$lhs->{text}, $lowop, $rhs->{text}];
	$sep = ' ';
	$opts = {maybe_parens => [$self, $cx, $lowprec]};
    }
    $opts->{block} = [$lhs, $rhs];
    return info_from_list($texts, $sep, $type, $opts);
}

sub baseop
{
    my($self, $op, $cx, $name) = @_;
    return info_from_text($self->keyword($name), 'baseop', {});
}

sub POSTFIX () { 1 }

# I couldn't think of a good short name, but this is the category of
# symbolic unary operators with interesting precedence

sub pfixop {
    my $self = shift;
    my($op, $cx, $name, $prec, $flags) = (@_, 0);
    my $kid = $op->first;
    $kid = $self->deparse($kid, $prec, $op);
    my $type = 'pfixop';
    my @texts;
    if ($flags & POSTFIX) {
	@texts = ($kid->{text}, $name);
	$type = 'pfixop_postfix';
    } elsif ($name eq '-' && $kid =~ /^[a-zA-Z](?!\w)/) {
	# avoid confusion with filetests
	$type = 'pfixop_minus';
	@texts = ($kid->{text}, '(', $name, ')');
    } else {
	@texts = ($name, $kid->{text});
    }

    return info_from_list(\@texts, '', $type,
			  {maybe_parens => [$self, $cx, $prec]});
}

sub mapop
{
    my($self, $op, $cx, $name) = @_;
    my $kid = $op->first; # this is the (map|grep)start

    my @other_ops = ($kid, $kid->first);
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
	other_ops => \@other_ops,
    };
    my $params = join(', ', @exprs_texts);
    $params = join(" ", @block_texts) . ' ' . $params if @block_texts;
    my @texts = $self->maybe_parens_func($name, $params, $cx, 5);
    return info_from_list(\@texts, '', 'mapop', $opts)
}

1;
