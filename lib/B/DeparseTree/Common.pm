# Copyright (c) 2015 Rocky Bernstein
package B::DeparseTree::Common;

use B qw(class opnumber OPpLVAL_INTRO OPf_SPECIAL OPf_KIDS);

use strict;
use warnings ();

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.0.0';
@ISA = qw(Exporter);
@EXPORT = qw(maybe_parens info_from_list info_from_text null new WARN_MASK
            indent_list indent indent_info parens_test print_protos style_opts);

BEGIN { for (qw[ pushmark ]) {
    eval "sub OP_\U$_ () { " . opnumber($_) . "}"
}}

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->{'cuddle'} = "\n";
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

# Initialise the contextual information, either from
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
    my ($self, $opts) = shift;
    my $opt;
    while (length($opt = substr($opts, 0, 1))) {
	if ($opt eq "C") {
	    $self->{'cuddle'} = " ";
	    $opts = substr($opts, 1);
	} elsif ($opt eq "i") {
	    $opts =~ s/^i(\d+)//;
	    $self->{'indent_size'} = $1;
	} elsif ($opt eq "T") {
	    $self->{'use_tabs'} = 1;
	    $opts = substr($opts, 1);
	} elsif ($opt eq "v") {
	    $opts =~ s/^v([^.]*)(.|$)//;
	    $self->{'ex_const'} = $1;
	}
    }
}

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

# Create an info structure from a list of strings
sub info_from_list($$$$)
{
    my ($texts, $sep, $type, $opts) = @_;
    my $text = join($sep, @$texts);
    my $info = {
	text => $text,
	texts => $texts,
	type => $type,
	sep => $sep,
    };
    foreach my $optname (qw(body other_ops)) {
	$info->{$optname} = $opts->{$optname} if $opts->{$optname};
    }
    if ($opts->{maybe_parens}) {
	my ($self, $cx, $prec) = @{$opts->{maybe_parens}};
	$info->{text} = $self->maybe_parens($info->{text}, $cx, $prec);
	$info->{maybe_parens} = {context => $cx , precidence => $prec};
    }
    return $info
}

# Create an info structure from a single string
sub info_from_text($$$)
{
    my ($text, $type, $opts) = @_;
    return info_from_list([$text], '', $type, $opts)
}

sub null
{
    my $op = shift;
    return class($op) eq "NULL";
}

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
    $info->{op} = $op;
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
	substr($text, 0, 1) == '(' and
	substr($text, -1) == ')' ) {
	$text = substr($text, 1, length($text) -2)
    }
    return $self->indent($text);
}


1;
