# B::DeparseTree.pm
# Copyright (c) 1998-2000, 2002, 2003, 2004, 2005, 2006 Stephen McCamant.
# Copyright (c) 2015 Rocky Bernstein
# All rights reserved.
# This module is free software; you can redistribute and/or modify
# it under the same terms as Perl itself.

# This is based on the module B::Deparse by Stephen McCamant.
# It has been extended save tree structure, and is addressible
# by opcode address.

# B::Parse in turn is based on the module of the same name by Malcolm Beattie,
# but essentially none of his code remains.

package B::DeparseTree;
use Carp;
use B qw(class main_root main_start main_cv svref_2object opnumber perlstring
	 OPf_WANT OPf_WANT_VOID OPf_WANT_SCALAR OPf_WANT_LIST
	 OPf_KIDS OPf_REF OPf_STACKED OPf_SPECIAL OPf_MOD
	 OPpLVAL_INTRO OPpOUR_INTRO OPpENTERSUB_AMPER OPpSLICE OPpCONST_BARE
	 OPpTRANS_SQUASH OPpTRANS_DELETE OPpTRANS_COMPLEMENT OPpTARGET_MY
	 OPpEXISTS_SUB OPpSORT_NUMERIC OPpSORT_INTEGER
	 OPpSORT_REVERSE
	 SVf_IOK SVf_NOK SVf_ROK SVf_POK SVpad_OUR SVf_FAKE SVs_RMG SVs_SMG
         CVf_METHOD CVf_LVALUE
	 PMf_KEEP PMf_GLOBAL PMf_CONTINUE PMf_EVAL PMf_ONCE
	 PMf_MULTILINE PMf_SINGLELINE PMf_FOLD PMf_EXTENDED);

$VERSION = '2.0';
use strict;
use vars qw/$AUTOLOAD/;
use warnings ();
require feature;

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

# See older source for inline comments recording changes prior to 2.0

# In order to test modulie, we run this over Perl test suite.
# Then we run the test on the deparsed code. Slick, eh?

# Here are B::Deparse failure notes:

# Current test.deparse failures
# comp/hints 6 - location of BEGIN blocks wrt. block openings
# run/switchI 1 - missing -I switches entirely
#    perl -Ifoo -e 'print @INC'
# op/caller 2 - warning mask propagates backwards before warnings::register
#    'use warnings; BEGIN {${^WARNING_BITS} eq "U"x12;} use warnings::register'
# op/getpid 2 - can't assign to shared my() declaration (threads only)
#    'my $x : shared = 5'
# op/override 7 - parens on overridden require change v-string interpretation
#    'BEGIN{*CORE::GLOBAL::require=sub {}} require v5.6'
#    c.f. 'BEGIN { *f = sub {0} }; f 2'
# op/pat 774 - losing Unicode-ness of Latin1-only strings
#    'use charnames ":short"; $x="\N{latin:a with acute}"'
# op/recurse 12 - missing parens on recursive call makes it look like method
#    'sub f { f($x) }'
# op/subst 90 - inconsistent handling of utf8 under "use utf8"
# op/taint 29 - "use re 'taint'" deparsed in the wrong place wrt. block open
# op/tiehandle compile - "use strict" deparsed in the wrong place
# uni/tr_ several
# ext/B/t/xref 11 - line numbers when we add newlines to one-line subs
# ext/Data/Dumper/t/dumper compile
# ext/DB_file/several
# ext/Encode/several
# ext/Ernno/Errno warnings
# ext/IO/lib/IO/t/io_sel 23
# ext/PerlIO/t/encoding compile
# ext/POSIX/t/posix 6
# ext/Socket/Socket 8
# ext/Storable/t/croak compile
# lib/Attribute/Handlers/t/multi compile
# lib/bignum/ several
# lib/charnames 35
# lib/constant 32
# lib/English 40
# lib/ExtUtils/t/bytes 4
# lib/File/DosGlob compile
# lib/Filter/Simple/t/data 1
# lib/Math/BigInt/t/constant 1
# lib/Net/t/config Deparse-warning
# lib/overload compile
# lib/Switch/ several
# lib/Symbol 4
# lib/Test/Simple several
# lib/Term/Complete
# lib/Tie/File/t/29_downcopy 5
# lib/vars 22

# Object fields (were globals):
#
# avoid_local:
# (local($a), local($b)) and local($a, $b) have the same internal
# representation but the short form looks better. We notice we can
# use a large-scale local when checking the list, but need to prevent
# individual locals too. This hash holds the addresses of OPs that
# have already had their local-ness accounted for. The same thing
# is done with my().
#
# curcv:
# CV for current sub (or main program) being deparsed
#
# curcvlex:
# Cached hash of lexical variables for curcv: keys are
# names prefixed with "m" or "o" (representing my/our), and
# each value is an array of pairs, indicating the cop_seq of scopes
# in which a var of that name is valid.
#
# curcop:
# COP for statement being deparsed
#
# curstash:
# name of the current package for deparsed code
#
# subs_todo:
# array of [cop_seq, CV, is_format?] for subs and formats we still
# want to deparse
#
# protos_todo:
# as above, but [name, prototype] for subs that never got a GV
#
# subs_done, forms_done:
# keys are addresses of GVs for subs and formats we've already
# deparsed (or at least put into subs_todo)
#
# subs_declared
# keys are names of subs for which we've printed declarations.
# That means we can omit parentheses from the arguments. It also means we
# need to put CORE:: on core functions of the same name.
#
# subs_deparsed
# Keeps track of fully qualified names of all deparsed subs.
#
# in_subst_repl
# True when deparsing the replacement part of a substitution.
#
# parens: -p
# linenums: -l
# copaddr: -c
# unquote: -q
# cuddle: ' ' or '\n', depending on -sC
# indent_size: -si
# use_tabs: -sT
# ex_const: -sv

# A little explanation of how precedence contexts and associativity
# work:
#
# deparse() calls each per-op subroutine with an argument $cx (short
# for context, but not the same as the cx* in the perl core), which is
# a number describing the op's parents in terms of precedence, whether
# they're inside an expression or at statement level, etc.  (see
# chart below). When ops with children call deparse on them, they pass
# along their precedence. Fractional values are used to implement
# associativity ('($x + $y) + $z' => '$x + $y + $y') and related
# parentheses hacks. The major disadvantage of this scheme is that
# it doesn't know about right sides and left sides, so say if you
# assign a listop to a variable, it can't tell it's allowed to leave
# the parens off the listop.

# Precedences:
# 26             [TODO] inside interpolation context ("")
# 25 left        terms and list operators (leftward)
# 24 left        ->
# 23 nonassoc    ++ --
# 22 right       **
# 21 right       ! ~ \ and unary + and -
# 20 left        =~ !~
# 19 left        * / % x
# 18 left        + - .
# 17 left        << >>
# 16 nonassoc    named unary operators
# 15 nonassoc    < > <= >= lt gt le ge
# 14 nonassoc    == != <=> eq ne cmp
# 13 left        &
# 12 left        | ^
# 11 left        &&
# 10 left        ||
#  9 nonassoc    ..  ...
#  8 right       ?:
#  7 right       = += -= *= etc.
#  6 left        , =>
#  5 nonassoc    list operators (rightward)
#  4 right       not
#  3 left        and
#  2 left        or xor
#  1             statement modifiers
#  0.5           statements, but still print scopes as do { ... }
#  0             statement level
# -1             format body

# Nonprinting characters with special meaning:
# \cS - steal parens (see maybe_parens_unop)
# \n - newline and indent
# \t - increase indent
# \b - decrease indent ('outdent')
# \f - flush left (no indent)
# \cK - kill following semicolon, if any

BEGIN { for (qw[ const stringify rv2sv list glob pushmark null]) {
    eval "sub OP_\U$_ () { " . opnumber($_) . "}"
}}

# _pessimise_walk(): recursively walk the optree of a sub,
# possibly undoing optimisations along the way.

sub _pessimise_walk {
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

	# pessimisations end here

	if (class($op) eq 'PMOP'
	    && ref($op->pmreplroot)
	    && ${$op->pmreplroot}
	    && $op->pmreplroot->isa( 'B::OP' ))
	{
	    $self-> _pessimise_walk($op->pmreplroot);
	}

	if ($op->flags & OPf_KIDS) {
	    $self-> _pessimise_walk($op->first);
	}

    }
}


# _pessimise_walk_exe(): recursively walk the op_next chain of a sub,
# possibly undoing optimisations along the way.

sub _pessimise_walk_exe {
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
	    $self->_pessimise_walk_exe($op->other, $visited);
	}
	elsif ($ppname eq "subst") {
	    $self->_pessimise_walk_exe($op->pmreplstart, $visited);
	}
	elsif ($ppname =~ /^(enter(loop|iter))$/) {
	    # redoop and nextop will already be covered by the main block
	    # of the loop
	    $self->_pessimise_walk_exe($op->lastop, $visited);
	}

	# pessimisations start here
    }
}

# Go through an optree and "remove" some optimisations by using an
# overlay to selectively modify or un-null some ops. Deparsing in the
# absence of those optimisations is then easier.
#
# Note that older optimisations are not removed, as Deparse was already
# written to recognise them before the pessimise/overlay system was added.

sub pessimise {
    my ($self, $root, $start) = @_;

    # walk tree in root-to-branch order
    $self->_pessimise_walk($root);

    my %visited;
    # walk tree in execution order
    $self->_pessimise_walk_exe($start, \%visited);
}


sub null {
    my $op = shift;
    return class($op) eq "NULL";
}

sub todo {
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

    $op = $op->first->first; # skip leavewrite, lineseq
    my $kid;
    while (not null $op) {
	$op = $op->sibling; # skip nextstate
	my @body;
	$kid = $op->first->sibling; # skip pushmark
	push @texts, "\f".$self->const_sv($kid)->PV;
	$kid = $kid->sibling;
	for (; not null $kid; $kid = $kid->sibling) {
	    push @body, $self->deparse($kid, -1, $op);
	    $body[-1] =~ s/;\z//;
	}
	push @texts, "\f".join(", ", @body)."\n" if @body;
	$op = $op->sibling;
    }

    $info->{text} = join("", @texts) . "\f.";
    $info->{texts} = \@texts;
    return $info;
}

sub next_todo {
    my ($self, $parent) = @_;
    my $ent = shift @{$self->{'subs_todo'}};
    my $cv = $ent->[1];
    my $gv = $cv->GV;
    my $name = $self->gv_name($gv);
    if ($ent->[2]) {
	my $info = $self->deparse_format($ent->[1], $cv);
	my $texts = ["format $name", "=", $info->{text} + "\n"];
	return {
	    body => $info,
	    texts => $texts,
	    type => 'format_todo',
	    text => join(" ", @$texts),
	};
    } else {
	$self->{'subs_declared'}{$name} = 1;
	if ($name eq "BEGIN") {
	    my $use_dec = $self->begin_is_use($cv);
	    if (defined ($use_dec) and $self->{'expand'} < 5) {
		return { text => '', type => 'begin_todo' } if 0 == length($use_dec);
		return { text => $use_dec, type => 'begin_todo' };
	    }
	}
	my $l = '';
	if ($self->{'linenums'}) {
	    my $line = $gv->LINE;
	    my $file = $gv->FILE;
	    $l = "\n# line $line \"$file\"\n";
	}
	my $p = '';
	if (class($cv->STASH) ne "SPECIAL") {
	    my $stash = $cv->STASH->NAME;
	    if ($stash ne $self->{'curstash'}) {
		$p = "package $stash;\n";
		$name = "$self->{'curstash'}::$name" unless $name =~ /::/;
		$self->{'curstash'} = $stash;
	    }
	    $name =~ s/^\Q$stash\E::(?!\z|.*::)//;
	}
	my $info = $self->deparse_sub($cv, $parent);
	my $texts = ["${p}${l}sub $name ", $info->{text}];
        return {
	    text => join('', @$texts),
	    texts => $texts,
	    type => 'sub_todo',
	    body => $info
	};
    }
}

# Return a "use" declaration for this BEGIN block, if appropriate
sub begin_is_use
{
    my ($self, $cv) = @_;
    my $root = $cv->ROOT;
    local @$self{qw'curcv curcvlex'} = ($cv);
    local $B::overlay = {};
    $self->pessimise($root, $cv->START);
    # require B::Debug;
    # B::walkoptree($cv->ROOT, "debug");
    my $lineseq = $root->first;
    return if $lineseq->name ne "lineseq";

    my $req_op = $lineseq->first->sibling;
    return if $req_op->name ne "require";

    my $module;
    if ($req_op->first->private & OPpCONST_BARE) {
	# Actually it should always be a bareword
	$module = $self->const_sv($req_op->first)->PV;
	$module =~ s[/][::]g;
	$module =~ s/.pm$//;
    }
    else {
	$module = $self->const($self->const_sv($req_op->first), 6);
    }

    my $version;
    my $version_op = $req_op->sibling;
    return if class($version_op) eq "NULL";
    if ($version_op->name eq "lineseq") {
	# We have a version parameter; skip nextstate & pushmark
	my $constop = $version_op->first->next->next;

	return unless $self->const_sv($constop)->PV eq $module;
	$constop = $constop->sibling;
	$version = $self->const_sv($constop);
	if (class($version) eq "IV") {
	    $version = $version->int_value;
	} elsif (class($version) eq "NV") {
	    $version = $version->NV;
	} elsif (class($version) ne "PVMG") {
	    # Includes PVIV and PVNV
	    $version = $version->PV;
	} else {
	    # version specified as a v-string
	    $version = 'v'.join '.', map ord, split //, $version->PV;
	}
	$constop = $constop->sibling;
	return if $constop->name ne "method_named";
	return if $self->const_sv($constop)->PV ne "VERSION";
    }

    $lineseq = $version_op->sibling;
    return if $lineseq->name ne "lineseq";
    my $entersub = $lineseq->first->sibling;
    if ($entersub->name eq "stub") {
	return "use $module $version ();\n" if defined $version;
	return "use $module ();\n";
    }
    return if $entersub->name ne "entersub";

    # See if there are import arguments
    my $args = '';

    my $svop = $entersub->first->sibling; # Skip over pushmark
    return unless $self->const_sv($svop)->PV eq $module;

    # Pull out the arguments
    for ($svop=$svop->sibling; $svop->name ne "method_named";
		$svop = $svop->sibling) {
	$args .= ", " if length($args);
	$args .= $self->deparse($svop, 6, $root);
    }

    my $use = 'use';
    my $method_named = $svop;
    return if $method_named->name ne "method_named";
    my $method_name = $self->const_sv($method_named)->PV;

    if ($method_name eq "unimport") {
	$use = 'no';
    }

    # Certain pragmas are dealt with using hint bits,
    # so we ignore them here
    if ($module eq 'strict' || $module eq 'integer'
	|| $module eq 'bytes' || $module eq 'warnings'
	|| $module eq 'feature') {
	return "";
    }

    if (defined $version && length $args) {
	return "$use $module $version ($args);\n";
    } elsif (defined $version) {
	return "$use $module $version;\n";
    } elsif (length $args) {
	return "$use $module ($args);\n";
    } else {
	return "$use $module;\n";
    }
}

sub stash_subs {
    my ($self, $pack, $seen) = @_;
    my (@ret, $stash);
    if (!defined $pack) {
	$pack = '';
	$stash = \%::;
    }
    else {
	$pack =~ s/(::)?$/::/;
	no strict 'refs';
	$stash = \%{"main::$pack"};
    }
    return
	if ($seen ||= {})->{
	    $INC{"overload.pm"} ? overload::StrVal($stash) : $stash
	   }++;
    my %stash = svref_2object($stash)->ARRAY;
    while (my ($key, $val) = each %stash) {
	my $class = class($val);
	if ($class eq "PV") {
	    # Just a prototype. As an ugly but fairly effective way
	    # to find out if it belongs here is to see if the AUTOLOAD
	    # (if any) for the stash was defined in one of our files.
	    my $A = $stash{"AUTOLOAD"};
	    if (defined ($A) && class($A) eq "GV" && defined($A->CV)
		&& class($A->CV) eq "CV") {
		my $AF = $A->FILE;
		next unless $AF eq $0 || exists $self->{'files'}{$AF};
	    }
	    push @{$self->{'protos_todo'}}, [$pack . $key, $val->PV];
	} elsif ($class eq "IV" && !($val->FLAGS & SVf_ROK)) {
	    # Just a name. As above.
	    # But skip proxy constant subroutines, as some form of perl-space
	    # visible code must have created them, be it a use statement, or
	    # some direct symbol-table manipulation code that we will Deparse
	    my $A = $stash{"AUTOLOAD"};
	    if (defined ($A) && class($A) eq "GV" && defined($A->CV)
		&& class($A->CV) eq "CV") {
		my $AF = $A->FILE;
		next unless $AF eq $0 || exists $self->{'files'}{$AF};
	    }
	    push @{$self->{'protos_todo'}}, [$pack . $key, undef];
	} elsif ($class eq "GV") {
	    if (class(my $cv = $val->CV) ne "SPECIAL") {
		next if $self->{'subs_done'}{$$val}++;
		next if $$val != ${$cv->GV};   # Ignore imposters
		$self->todo($cv, 0);
	    }
	    if (class(my $cv = $val->FORM) ne "SPECIAL") {
		next if $self->{'forms_done'}{$$val}++;
		next if $$val != ${$cv->GV};   # Ignore imposters
		$self->todo($cv, 1);
	    }
	    if (class($val->HV) ne "SPECIAL" && $key =~ /::$/) {
		$self->stash_subs($pack . $key, $seen);
	    }
	}
    }
}

sub print_protos {
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

sub style_opts {
    my $self = shift;
    my $opts = shift;
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
    $self->{'copaddr'} = 0;
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
	} elsif ($arg eq "-c") {
	    $self->{'linenums'} = 1;
	    $self->{'copaddr'} = 1;
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
sub init {
    my $self = shift;

    $self->{'arybase'}  = $self->{'ambient_arybase'};
    $self->{'warnings'} = defined ($self->{'ambient_warnings'})
				? $self->{'ambient_warnings'} & WARN_MASK
				: undef;
    $self->{'hints'}    = $self->{'ambient_hints'};
    $self->{'hints'} &= 0xFF if $] < 5.009;
    $self->{'hinthash'} = $self->{'ambient_hinthash'};

    # also a convenient place to clear out subs_declared
    delete $self->{'subs_declared'};
}

sub compile {
    my(@args) = @_;
    return sub {
	my $self = B::DeparseTree->new(@args);
	# First deparse command-line args
	if (defined $^I) { # deparse -i
	    print q(BEGIN { $^I = ).perlstring($^I).qq(; }\n);
	}
	if ($^W) { # deparse -w
	    print qq(BEGIN { \$^W = $^W; }\n);
	}
	if ($/ ne "\n" or defined $O::savebackslash) { # deparse -l and -0
	    my $fs = perlstring($/) || 'undef';
	    my $bs = perlstring($O::savebackslash) || 'undef';
	    print qq(BEGIN { \$/ = $fs; \$\\ = $bs; }\n);
	}
	my @BEGINs  = B::begin_av->isa("B::AV") ? B::begin_av->ARRAY : ();
	my @UNITCHECKs = B::unitcheck_av->isa("B::AV")
	    ? B::unitcheck_av->ARRAY
	    : ();
	my @CHECKs  = B::check_av->isa("B::AV") ? B::check_av->ARRAY : ();
	my @INITs   = B::init_av->isa("B::AV") ? B::init_av->ARRAY : ();
	my @ENDs    = B::end_av->isa("B::AV") ? B::end_av->ARRAY : ();
	for my $block (@BEGINs, @UNITCHECKs, @CHECKs, @INITs, @ENDs) {
	    $self->todo($block, 0);
	}
	$self->stash_subs();
	local($SIG{"__DIE__"}) =
	  sub {
	      if ($self->{'curcop'}) {
		  my $cop = $self->{'curcop'};
		  my($line, $file) = ($cop->line, $cop->file);
		  print STDERR "While deparsing $file near line $line,\n";
	      }
	    };
	$self->{'curcv'} = main_cv;
	$self->{'curcvlex'} = undef;
	print $self->print_protos;
	@{$self->{'subs_todo'}} =
	  sort {$a->[0] <=> $b->[0]} @{$self->{'subs_todo'}};
	my $root = main_root;
	local $B::overlay = {};
	unless (null $root) {
	    $self->pessimise($root, main_start);
	    my $deparsed = $self->deparse_root($root);
	    print $self->indent_info($deparsed), "\n";
	}
	my @texts;
	while (scalar(@{$self->{'subs_todo'}})) {
	    push @texts, $self->next_todo;
	}
	print $self->indent(join("", @texts)), "\n" if @texts;

	# Print __DATA__ section, if necessary
	no strict 'refs';
	my $laststash = defined $self->{'curcop'}
	    ? $self->{'curcop'}->stash->NAME : $self->{'curstash'};
	if (defined *{$laststash."::DATA"}{IO}) {
	    print "package $laststash;\n"
		unless $laststash eq $self->{'curstash'};
	    print "__DATA__\n";
	    print readline(*{$laststash."::DATA"});
	}
    }
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

# Possibly add () around $text depending on precidence $prec and
# context $cx. We return a string.
sub maybe_parens($$$$)
{
    my($self, $text, $cx, $prec) = @_;
    if ($prec < $cx
	# unary ops nest just fine
	or $prec == $cx and $cx != 4 and $cx != 16 and $cx != 21
	or $self->{'parens'}) {
	$text = "($text)";
	# In a unop, let parent reuse our parens; see maybe_parens_unop
	$text = "\cS" . $text if $cx == 16;
	return $text;
    } else {
	return $text;
    }
}

# Create an info structure from a list of strings
sub info_from_list($$$$) {
    my ($texts, $sep, $type, $opts) = @_;
    my $text = join($sep, @$texts);
    my $info = {
	text => $text,
	texts => $texts,
	type => $type,
	sep => $sep,
    };
    foreach my $optname (qw(body other_ops)) {
	$info->{$optname} = $opts->{$optname};
    }
    if ($opts->{maybe_parens}) {
	my ($self, $cx, $prec) = @{$opts->{maybe_parens}};
	$info->{text} = $self->maybe_parens($info->{text}, $cx, $prec);
	$info->{cx} = $cx;
	$info->{prec} = $prec;
    }
    return $info
}

# Create an info structure from a single string
sub info_from_text($$$) {
    my ($text, $type, $opts) = @_;
    return info_from_list([$text], '', $type, $opts)
}

my %strict_bits = do {
    local $^H;
    map +($_ => strict::bits($_)), qw/refs subs vars/
};

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

# This method is the inner loop, so try to keep it simple
sub deparse {
    my $self = shift;
    my($op, $cx, $parent) = @_;

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


# Deparse a subroutine
sub deparse_sub {
    my ($self, $cv, $parent) = @_;
    my $proto = "";
    Carp::confess("NULL in deparse_sub") if !defined($cv) || $cv->isa("B::NULL");
    Carp::confess("SPECIAL in deparse_sub") if $cv->isa("B::SPECIAL");
    local $self->{'curcop'} = $self->{'curcop'};
    if ($cv->FLAGS & SVf_POK) {
	$proto = "(". $cv->PV . ") ";
    }
    if ($cv->CvFLAGS & (CVf_METHOD|CVf_LOCKED|CVf_LVALUE)) {
        $proto .= ": ";
        $proto .= "lvalue " if $cv->CvFLAGS & CVf_LVALUE;
        $proto .= "locked " if $cv->CvFLAGS & CVf_LOCKED;
        $proto .= "method " if $cv->CvFLAGS & CVf_METHOD;
    }

    local($self->{'curcv'}) = $cv;
    local($self->{'curcvlex'});
    local(@$self{qw'curstash warnings hints hinthash'})
		= @$self{qw'curstash warnings hints hinthash'};

    my $root = $cv->ROOT;
    my $body;

    my $info = {
	op => $root,
	cop => undef,
	parent => $parent,
	type => 'sub',
    };

    local $B::overlay = {};
    if (not null $root) {
	$self->pessimise($root, $cv->START);
	my $lineseq = $root->first;
	if ($lineseq->name eq "lineseq") {
	    my @ops;
	    for(my $o=$lineseq->first; $$o; $o=$o->sibling) {
		push @ops, $o;
	    }
	    $body = $self->lineseq($root, 0, @ops);
	    my $scope_en = $self->find_scope_en($lineseq);
	}
	else {
	    $body = $self->deparse($root->first, 0, $root);
	}
	my @texts = ("{\n\t", $body->{text}, "\n\b}");
	unshift @texts, $proto if $proto;
	$info->{body} = $body;
	$info->{texts} = \@texts;
	$info->{sep} = '';
	$info->{text} = join('', @texts);
    }
    else {
	my $sv = $cv->const_sv;
	if ($$sv) {
	    # uh-oh. inlinable sub... format it differently
	    my @texts = ("{", $self->const($sv, 0)->{text}, "}");
	    unshift @texts, $proto if $proto;
	    $info = info_from_list \@texts, '', 'sub_const', {};
	} else { # XSUB? (or just a declaration)
	    my @texts = ();
	    @texts = push @texts, $proto if $proto;
	    $info = info_from_list \@texts, ' ', 'sub_decl', {};
	}
    }

    $self->{optree}{$$root} = $info;
    return $info;
}

sub is_scope {
    my $op = shift;
    return $op->name eq "leave" || $op->name eq "scope"
      || $op->name eq "lineseq"
	|| ($op->name eq "null" && class($op) eq "UNOP"
	    && (is_scope($op->first) || $op->first->name eq "enter"));
}

sub is_state {
    my $name = $_[0]->name;
    return $name eq "nextstate" || $name eq "dbstate" || $name eq "setstate";
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

sub is_scalar {
    my $op = shift;
    return ($op->name eq "rv2sv" or
	    $op->name eq "padsv" or
	    $op->name eq "gv" or # only in array/hash constructs
	    $op->flags & OPf_KIDS && !null($op->first)
	      && $op->first->name eq "gvsv");
}

# Sort of like maybe_parens in that we may possibly add ().  However we take
# an op rather than text, and return a tree node. Also, we get around
# the 'if it looks like a function' rule.
sub maybe_parens_unop($self, $name, $op, $cx, $parent)
{
    my $self = shift;
    my($name, $op, $cx, $parent) = @_;
    my $info =  $self->deparse($op, 1, $parent);
    my $text = $info->{text};
    if ($name eq "umask" && $info->{text} =~ /^\d+$/) {
	$text = sprintf("%#o", $text);
    }
    if ($cx > 16 or $self->{'parens'}) {
	return info_from_list([$self->keyword($name), '(', $text, ')'], '',
			      'maybe_parens_unop_parens', {body => [$info]});
    } else {
	$name = $self->keyword($name);
	if (substr($text, 0, 1) eq "\cS") {
	    # use op's parens
	    return info_from_list([$name, substr($text, 1)], '',
				  'maybe_parens_unop_cS', {body => [$info]});
	} elsif (substr($text, 0, 1) eq "(") {
	    # avoid looks-like-a-function trap with extra parens
	    # ('+' can lead to ambiguities)
	    return info_from_list([$name, '(', $text, ')'], '',
				  "maybe_parens_unop_fn", {});
	} else {
	    return info_from_list([$name,  $text], '',
				  'maybe_parens_unop', {});
	}
    }
    Carp::confess("unhandled condition in maybe_parens_unop");
}

sub maybe_parens_func($$$$$)
{
    my($self, $func, $params, $cx, $prec) = @_;
    if ($prec <= $cx or substr($params, 0, 1) eq "(" or $self->{'parens'}) {
	return ("$func", "(", $params, ")");
    } else {
	return ($func, ' ', $params);
    }
}

sub dedup_parens_func
{
    my $sub_name = shift;
    my @args = @_;
    if (scalar @args == 1 && substr($args[0], 0, 1) eq '(' &&
	substr($args[0], -1, 1) eq ')') {
	return ($sub_name, $args[0]);
    } else {
	return ($sub_name, '(', @args, ')', );
    }
}

sub maybe_local_str($$$$)
{
    my($self, $op, $cx, $text) = @_;
    my $our_intro = ($op->name =~ /^(gv|rv2)[ash]v$/) ? OPpOUR_INTRO : 0;
    if ($op->private & (OPpLVAL_INTRO|$our_intro)
	and not $self->{'avoid_local'}{$$op}) {
	my $our_local = ($op->private & OPpLVAL_INTRO) ? "local" : "our";
	if( $our_local eq 'our' ) {
	    if ( $text !~ /^\W(\w+::)*\w+\z/
	     and !utf8::decode($text) || $text !~ /^\W(\w+::)*\w+\z/
	    ) {
		die "Unexpected our($text)\n";
	    }
	    $text =~ s/(\w+::)+//;
	}
        if (want_scalar($op)) {
	    return info_from_list([$our_local, $text], ' ', 'maybe_local_scalar', {});
	} else {
	    my @texts = $self->maybe_parens_func($our_local, $text, $cx, 16);
	    return info_from_list(\@texts, '', 'maybe_local_array', {});
	}
    } else {
	return info_from_text($text, 'maybe_local', {});
    }
}

sub maybe_local {
    my($self, $op, $cx, $var_info) = @_;
    $var_info->{parent} = $$op;
    return maybe_local_str($self, $op, $cx, $var_info->{text});
}

sub maybe_targmy {
    my $self = shift;
    my($op, $cx, $func, @args) = @_;
    if ($op->private & OPpTARGET_MY) {
	my $var = $self->padname($op->targ);
	my $val = $func->($self, $op, 7, @args);
	return $self->maybe_parens("$var = $val", $cx, 7);
    } else {
	return $func->($self, $op, $cx, @args);
    }
}

sub padname_sv {
    my $self = shift;
    my $targ = shift;
    return $self->{'curcv'}->PADLIST->ARRAYelt(0)->ARRAYelt($targ);
}

sub maybe_my {
    my $self = shift;
    my($op, $cx, $text, $forbid_parens) = @_;
    if ($op->private & OPpLVAL_INTRO and not $self->{'avoid_local'}{$$op}) {
	my $my_str = $op->private & OPpPAD_STATE
	    ? $self->keyword("state")
	    : "my";
	if ($forbid_parens || want_scalar($op)) {
	    return info_from_list([$my_str,  $text], ' ', 'maybe_my_no_parens', {});
	} else {
	    return info_from_list([$my_str,  $text], ' ',
				  'maybe_my_parens',
				  {maybe_parens => [$self, $cx, 16]});
	}
    } else {
	return info_from_text($text, 'maybe_my_avoid_local', {});
    }
}

# The following OPs don't have functions:

# pp_padany -- does not exist after parsing

sub AUTOLOAD {
    if ($AUTOLOAD =~ s/^.*::pp_//) {
	warn "unexpected OP_".uc $AUTOLOAD;
	return "XXX";
    } else {
	die "Undefined subroutine $AUTOLOAD called";
    }
}

sub DESTROY {}	#	Do not AUTOLOAD

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
	$text =~ s/^\((.+)\)$/$1/;
	$info->{type} = $op->name;
	$info->{op} = $op;
	if ($parent) {
	    Carp::confess("nonref parent, op: $op->name") if !ref($parent);
	    $info->{parent} = $$parent ;
	}
	if ($info->{body}) {
	    foreach my $bod (@{$info->{body}}) {
		$bod->{parent} = $$op;
	    }
	}
	$self->{optree}{$$op} = $info;
	$info->{text} = $text;
	push @$exprs, $info;
    };
    return $self->walk_lineseq($root, \@ops, $fn);
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
		return $self->deparse($top->first, 1, $top) . " while 1";
	    }
	    my $cond = $top->first;
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
	    @texts = ('do ', '{\n\t', @texts, "\n\b");
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

sub pp_scope { scopeop(0, @_); }
sub pp_lineseq { scopeop(0, @_); }
sub pp_leave { scopeop(1, @_); }

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
	$info->{parent} = $$parent unless $info->{parent};
	$self->{optree}{$$op} = $info;
	$info->{text} = $text;
	push @$exprs, $info;
    };
    return $self->walk_lineseq($op, \@ops, $fn);
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

# The BEGIN {} is used here because otherwise this code isn't executed
# when you run B::Deparse on itself.
my %globalnames;
BEGIN { map($globalnames{$_}++, "SIG", "STDIN", "STDOUT", "STDERR", "INC",
	    "ENV", "ARGV", "ARGVOUT", "_"); }

sub gv_name {
    my $self = shift;
    my $gv = shift;
    my $raw = shift;
    Carp::confess() unless ref($gv) eq "B::GV";
    my $stash = $gv->STASH->NAME;
    my $name = $raw ? $gv->NAME : $gv->SAFENAME;
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

# Return the name to use for a stash variable.
# If a lexical with the same name is in scope, or
# if strictures are enabled, it may need to be
# fully-qualified.
sub stash_variable {
    my ($self, $prefix, $name, $cx) = @_;

    return "$prefix$name" if $name =~ /::/;

    unless ($prefix eq '$' || $prefix eq '@' || #'
	    $prefix eq '%' || $prefix eq '$#') {
	return "$prefix$name";
    }

    if ($name =~ /^[^\w+-]$/) {
      if (defined $cx && $cx == 26) {
	if ($prefix eq '@') {
	    return "$prefix\{$name}";
	}
	elsif ($name eq '#') { return '${#}' } #  "${#}a" vs "$#a"
      }
      if ($prefix eq '$#') {
	return "\$#{$name}";
      }
    }

    return $prefix . $self->maybe_qualify($prefix, $name);
}

# Return just the name, without the prefix.  It may be returned as a quoted
# string.  The second return value is a boolean indicating that.
sub stash_variable_name {
    my($self, $prefix, $gv) = @_;
    my $name = $self->gv_name($gv, 1);
    $name = $self->maybe_qualify($prefix,$name);
    if ($name =~ /^(?:\S|(?!\d)[\ca-\cz]?(?:\w|::)*|\d+)\z/) {
	$name =~ s/^([\ca-\cz])/'^'.($1|'@')/e;
	$name =~ /^(\^..|{)/ and $name = "{$name}";
	return $name, 0; # not quoted
    }
    else {
	single_delim("q", "'", $name), 1;
    }
}

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

sub lex_in_scope {
    my ($self, $name, $our) = @_;
    substr $name, 0, 0, = $our ? 'o' : 'm'; # our/my
    $self->populate_curcvlex() if !defined $self->{'curcvlex'};

    return 0 if !defined($self->{'curcop'});
    my $seq = $self->{'curcop'}->cop_seq;
    return 0 if !exists $self->{'curcvlex'}{$name};
    for my $a (@{$self->{'curcvlex'}{$name}}) {
	my ($st, $en) = @$a;
	return 1 if $seq > $st && $seq <= $en;
    }
    return 0;
}

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
	    my ($seq_st, $seq_en) =
		($ns[$i]->FLAGS & SVf_FAKE)
		    ? (0, 999999)
		    : ($ns[$i]->COP_SEQ_RANGE_LOW, $ns[$i]->COP_SEQ_RANGE_HIGH);

	    push @{$self->{'curcvlex'}{
			($ns[$i]->FLAGS & SVpad_OUR ? 'o' : 'm') . $name
		  }}, [$seq_st, $seq_en];
	}
    }
}

sub find_scope_st { ((find_scope(@_))[0]); }
sub find_scope_en { ((find_scope(@_))[1]); }

# Recurses down the tree, looking for pad variable introductions and COPs
sub find_scope {
    my ($self, $op, $scope_st, $scope_en) = @_;
    carp("Undefined op in find_scope") if !defined $op;
    return ($scope_st, $scope_en) unless $op->flags & OPf_KIDS;

    my @queue = ($op);
    while(my $op = shift @queue ) {
	for (my $o=$op->first; $$o; $o=$o->sibling) {
	    if ($o->name =~ /^pad.v$/ && $o->private & OPpLVAL_INTRO) {
		my $s = int($self->padname_sv($o->targ)->COP_SEQ_RANGE_LOW);
		my $e = $self->padname_sv($o->targ)->COP_SEQ_RANGE_HIGH;
		$scope_st = $s if !defined($scope_st) || $s < $scope_st;
		$scope_en = $e if !defined($scope_en) || $e > $scope_en;
		return ($scope_st, $scope_en);
	    }
	    elsif (is_state($o)) {
		my $c = $o->cop_seq;
		$scope_st = $c if !defined($scope_st) || $c < $scope_st;
		$scope_en = $c if !defined($scope_en) || $c > $scope_en;
		return ($scope_st, $scope_en);
	    }
	    elsif ($o->flags & OPf_KIDS) {
		unshift (@queue, $o);
	    }
	}
    }

    return ($scope_st, $scope_en);
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

sub seq_subs {
    my ($self, $seq) = @_;
    my @text;
    # push @text, "# ($seq)\n";

    return "" if !defined $seq;
    while (scalar(@{$self->{'subs_todo'}})
	   and $seq > $self->{'subs_todo'}[0][0]) {
	push @text, $self->next_todo;
    }
    return @text;
}

sub _features_from_bundle {
    my ($hints, $hh) = @_;
    foreach (@{$feature::feature_bundle{@feature::hint_bundles[$hints >> $feature::hint_shift]}}) {
	$hh->{$feature::feature{$_}} = 1;
    }
    return $hh;
}

# Notice how subs and formats are inserted between statements here;
# also $[ assignments and pragmas.
sub pp_nextstate {
    my $self = shift;
    my($op, $cx) = @_;
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

    my $hints = $] < 5.008009 ? $op->private : $op->hints;
    my $old_hints = $self->{'hints'};
    if ($self->{'hints'} != $hints) {
	push @text, declare_hints($self->{'hints'}, $hints);
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
		push @text, "no feature;\n",
			    "use feature ':$bundle';\n";
	    }
	}
    }

    if ($] > 5.009) {
	push @text, declare_hinthash(
	    $self->{'hinthash'}, $newhh,
	    $self->{indent_size}, $self->{hints},
	);
	$self->{'hinthash'} = $newhh;
    }

    # This should go after of any branches that add statements, to
    # increase the chances that it refers to the same line it did in
    # the original program.
    if ($self->{'linenums'}) {
	my $line = sprintf("\n# line %s '%s'", $op->line, $op->file);
	$line .= sprintf(" 0x%x", $$op) if $self->{'copaddr'};
	push @text, $line . "\cK\n";
    }

    push @text, $op->label . ": " if $op->label;

    return {
	op => $op,
	texts => \@text,
	text => join("", @text)
    }
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

sub declare_hints {
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

sub hint_pragmas {
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

sub pp_dbstate { pp_nextstate(@_) }
sub pp_setstate { pp_nextstate(@_) }

sub pp_unstack {
    # see also leaveloop
    return info_from_text('', 'unstack', {});
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

sub keyword {
    my $self = shift;
    my $name = shift;
    return $name if $name =~ /^CORE::/; # just in case
    if (exists $feature_keywords{$name}) {
	my $hh;
	my $hints = $self->{hints} & $feature::hint_mask;
	if ($hints && $hints != $feature::hint_mask) {
	    $hh = _features_from_bundle($hints);
	}
	elsif ($hints) { $hh = $self->{'hinthash'} }
	return "CORE::$name"
	 if !$hh
	 || !$hh->{"feature_$feature_keywords{$name}"}
    }
    if ($strong_proto_keywords{$name}
        || ($name !~ /^(?:chom?p|do|exec|glob|s(?:elect|ystem))\z/
	    && !defined eval{prototype "CORE::$name"})
    ) { return $name }
    if (
	exists $self->{subs_declared}{$name}
	 or
	exists &{"$self->{curstash}::$name"}
    ) {
	return "CORE::$name"
    }
    return $name;
}

sub baseop {
    my $self = shift;
    my($op, $cx, $name) = @_;
    return $self->keyword($name);
}

sub pp_stub {
    my $self = shift;
    my($op, $cx, $name) = @_;
    if ($cx >= 1) {
	return info_from_list(["(", ")"], '', 'stub_cx', {});
    }
    else {
	return info_from_list(["(", ")", ';'], '', 'stub', {});
    }
}
sub pp_wantarray { baseop(@_, "wantarray") }
sub pp_fork { baseop(@_, "fork") }
sub pp_wait { maybe_my(@_, \&baseop, "wait") }
sub pp_getppid { maybe_targmy(@_, \&baseop, "getppid") }
sub pp_time { maybe_targmy(@_, \&baseop, "time") }
sub pp_tms { baseop(@_, "times") }
sub pp_ghostent { baseop(@_, "gethostent") }
sub pp_gnetent { baseop(@_, "getnetent") }
sub pp_gprotoent { baseop(@_, "getprotoent") }
sub pp_gservent { baseop(@_, "getservent") }
sub pp_ehostent { baseop(@_, "endhostent") }
sub pp_enetent { baseop(@_, "endnetent") }
sub pp_eprotoent { baseop(@_, "endprotoent") }
sub pp_eservent { baseop(@_, "endservent") }
sub pp_gpwent { baseop(@_, "getpwent") }
sub pp_spwent { baseop(@_, "setpwent") }
sub pp_epwent { baseop(@_, "endpwent") }
sub pp_ggrent { baseop(@_, "getgrent") }
sub pp_sgrent { baseop(@_, "setgrent") }
sub pp_egrent { baseop(@_, "endgrent") }
sub pp_getlogin { baseop(@_, "getlogin") }

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

    my $text = $self->maybe_parens(join('', @texts), $cx, $prec);
    return {
	text => $text,
	texts => @texts,
	type => $type
    };
}

sub pp_preinc { pfixop(@_, "++", 23) }
sub pp_predec { pfixop(@_, "--", 23) }
sub pp_postinc { maybe_targmy(@_, \&pfixop, "++", 23, POSTFIX) }
sub pp_postdec { maybe_targmy(@_, \&pfixop, "--", 23, POSTFIX) }
sub pp_i_preinc { pfixop(@_, "++", 23) }
sub pp_i_predec { pfixop(@_, "--", 23) }
sub pp_i_postinc { maybe_targmy(@_, \&pfixop, "++", 23, POSTFIX) }
sub pp_i_postdec { maybe_targmy(@_, \&pfixop, "--", 23, POSTFIX) }
sub pp_complement { maybe_targmy(@_, \&pfixop, "~", 21) }

sub pp_negate { maybe_targmy(@_, \&real_negate) }
sub real_negate {
    my $self = shift;
    my($op, $cx) = @_;
    if ($op->first->name =~ /^(i_)?negate$/) {
	# avoid --$x
	$self->pfixop($op, $cx, "-", 21.5);
    } else {
	$self->pfixop($op, $cx, "-", 21);
    }
}
sub pp_i_negate { pp_negate(@_) }

sub pp_not {
    my $self = shift;
    my($op, $cx) = @_;
    if ($cx <= 4) {
	$self->listop($op, $cx, "not", $op->first);
    } else {
	$self->pfixop($op, $cx, "!", 21);
    }
}

sub unop {
    my $self = shift;
    my($op, $cx, $name, $nollafr) = @_;
    my $kid;
    if ($op->flags & OPf_KIDS) {
	$kid = $op->first;
 	if (not $name) {
 	    # this deals with 'boolkeys' right now
 	    return $self->deparse($kid, $cx, $op);
 	}
	my $builtinname = $name;
	$builtinname =~ /^CORE::/ or $builtinname = "CORE::$name";
	if (defined prototype($builtinname)
	   && prototype($builtinname) =~ /^;?\*/
	   && $kid->name eq "rv2gv") {
	    $kid = $kid->first;
	}

	if ($nollafr) {
	    $kid = $self->deparse($kid, 16, $op);
	    ($kid->{text}) =~ s/^\cS//;
	    my @texts = ($self->keyword($name), ' ', $kid->{text});
	    my $text = join('', @texts);
	    $text = $self->maybe_parens($text, $cx, 16);
	    return {
		text => $text,
		texts => @texts,
		type => 'unop_noallafr',
		body => [$kid],
		type => 'unop_nollafr',
	    };
	}
	return $self->maybe_parens_unop($name, $kid, $cx, $op);
    } else {
	my @texts = ($self->keyword($name));
	push @texts, '()' if $op->flags & OPf_SPECIAL;
	my $text = $self->maybe_parens(join('', @texts), $cx, 16);
	return {
	    text => $text,
	    texts => \@texts,
	    type => 'unop_nokid',
	};
    }
}

sub pp_chop { maybe_targmy(@_, \&unop, "chop") }
sub pp_chomp { maybe_targmy(@_, \&unop, "chomp") }
sub pp_schop { maybe_targmy(@_, \&unop, "chop") }
sub pp_schomp { maybe_targmy(@_, \&unop, "chomp") }
sub pp_defined { unop(@_, "defined") }
sub pp_undef { unop(@_, "undef") }
sub pp_study { unop(@_, "study") }
sub pp_ref { unop(@_, "ref") }
sub pp_pos { maybe_local(@_, unop(@_, "pos")) }

sub pp_sin { maybe_targmy(@_, \&unop, "sin") }
sub pp_cos { maybe_targmy(@_, \&unop, "cos") }
sub pp_rand { maybe_targmy(@_, \&unop, "rand") }
sub pp_srand { unop(@_, "srand") }
sub pp_exp { maybe_targmy(@_, \&unop, "exp") }
sub pp_log { maybe_targmy(@_, \&unop, "log") }
sub pp_sqrt { maybe_targmy(@_, \&unop, "sqrt") }
sub pp_int { maybe_targmy(@_, \&unop, "int") }
sub pp_hex { maybe_targmy(@_, \&unop, "hex") }
sub pp_oct { maybe_targmy(@_, \&unop, "oct") }
sub pp_abs { maybe_targmy(@_, \&unop, "abs") }

sub pp_length { maybe_targmy(@_, \&unop, "length") }
sub pp_ord { maybe_targmy(@_, \&unop, "ord") }
sub pp_chr { maybe_targmy(@_, \&unop, "chr") }

sub pp_each { unop(@_, "each") }
sub pp_values { unop(@_, "values") }
sub pp_keys { unop(@_, "keys") }
{ no strict 'refs'; *{"pp_r$_"} = *{"pp_$_"} for qw< keys each values >; }
sub pp_boolkeys
{
    # no name because its an optimisation op that has no keyword
    unop(@_,"");
}
sub pp_aeach { unop(@_, "each") }
sub pp_avalues { unop(@_, "values") }
sub pp_akeys { unop(@_, "keys") }
sub pp_pop { unop(@_, "pop") }
sub pp_shift { unop(@_, "shift") }

sub pp_caller { unop(@_, "caller") }
sub pp_reset { unop(@_, "reset") }
sub pp_exit { unop(@_, "exit") }
sub pp_prototype { unop(@_, "prototype") }

sub pp_close { unop(@_, "close") }
sub pp_fileno { unop(@_, "fileno") }
sub pp_umask { unop(@_, "umask") }
sub pp_untie { unop(@_, "untie") }
sub pp_tied { unop(@_, "tied") }
sub pp_dbmclose { unop(@_, "dbmclose") }
sub pp_getc { unop(@_, "getc") }
sub pp_eof { unop(@_, "eof") }
sub pp_tell { unop(@_, "tell") }
sub pp_getsockname { unop(@_, "getsockname") }
sub pp_getpeername { unop(@_, "getpeername") }

sub pp_chdir { maybe_targmy(@_, \&unop, "chdir") }
sub pp_chroot { maybe_targmy(@_, \&unop, "chroot") }
sub pp_readlink { unop(@_, "readlink") }
sub pp_rmdir { maybe_targmy(@_, \&unop, "rmdir") }
sub pp_readdir { unop(@_, "readdir") }
sub pp_telldir { unop(@_, "telldir") }
sub pp_rewinddir { unop(@_, "rewinddir") }
sub pp_closedir { unop(@_, "closedir") }
sub pp_getpgrp { maybe_targmy(@_, \&unop, "getpgrp") }
sub pp_localtime { unop(@_, "localtime") }
sub pp_gmtime { unop(@_, "gmtime") }
sub pp_alarm { unop(@_, "alarm") }
sub pp_sleep { maybe_targmy(@_, \&unop, "sleep") }

sub pp_dofile
{
    my $code = unop(@_, "do", 1); # llafr does not apply
    if ($code =~ s/^((?:CORE::)?do) \{/$1({/) { $code .= ')' }
    $code;
}

sub pp_entereval
{
    unop(
      @_,
      $_[1]->private & OPpEVAL_BYTES ? $_[0]->keyword('evalbytes') : "eval"
    )
}

sub pp_ghbyname { unop(@_, "gethostbyname") }
sub pp_gnbyname { unop(@_, "getnetbyname") }
sub pp_gpbyname { unop(@_, "getprotobyname") }
sub pp_shostent { unop(@_, "sethostent") }
sub pp_snetent { unop(@_, "setnetent") }
sub pp_sprotoent { unop(@_, "setprotoent") }
sub pp_sservent { unop(@_, "setservent") }
sub pp_gpwnam { unop(@_, "getpwnam") }
sub pp_gpwuid { unop(@_, "getpwuid") }
sub pp_ggrnam { unop(@_, "getgrnam") }
sub pp_ggrgid { unop(@_, "getgrgid") }

sub pp_lock { unop(@_, "lock") }

sub pp_continue { unop(@_, "continue"); }
sub pp_break { unop(@_, "break"); }

sub givwhen
{
    my $self = shift;
    my($op, $cx, $givwhen) = @_;

    my $enterop = $op->first;
    my ($head, $block);
    if ($enterop->flags & OPf_SPECIAL) {
	$head = $self->keyword("default");
	$block = $self->deparse($enterop->first, 0, $enterop);
    }
    else {
	my $cond = $enterop->first;
	my $cond_str = $self->deparse($cond, 1, $enterop);
	$head = "$givwhen ($cond_str)";
	$block = $self->deparse($cond->sibling, 0, $enterop);
    }

    return "$head {\n".
	"\t$block\n".
	"\b}\cK";
}

sub pp_leavegiven { givwhen(@_, $_[0]->keyword("given")); }
sub pp_leavewhen  { givwhen(@_, $_[0]->keyword("when")); }

sub pp_exists
{
    my $self = shift;
    my($op, $cx) = @_;
    my $arg;
    if ($op->private & OPpEXISTS_SUB) {
	# Checking for the existence of a subroutine
	return $self->maybe_parens_func("exists",
				$self->pp_rv2cv($op->first, 16), $cx, 16);
    }
    if ($op->flags & OPf_SPECIAL) {
	# Array element, not hash element
	return $self->maybe_parens_func("exists",
				$self->pp_aelem($op->first, 16), $cx, 16);
    }
    return $self->maybe_parens_func("exists", $self->pp_helem($op->first, 16),
				    $cx, 16);
}

sub pp_delete
{
    my $self = shift;
    my($op, $cx) = @_;
    my $arg;
    if ($op->private & OPpSLICE) {
	if ($op->flags & OPf_SPECIAL) {
	    # Deleting from an array, not a hash
	    return $self->maybe_parens_func("delete",
					$self->pp_aslice($op->first, 16),
					$cx, 16);
	}
	return $self->maybe_parens_func("delete",
					$self->pp_hslice($op->first, 16),
					$cx, 16);
    } else {
	if ($op->flags & OPf_SPECIAL) {
	    # Deleting from an array, not a hash
	    return $self->maybe_parens_func("delete",
					$self->pp_aelem($op->first, 16),
					$cx, 16);
	}
	return $self->maybe_parens_func("delete",
					$self->pp_helem($op->first, 16),
					$cx, 16);
    }
}

sub pp_require
{
    my $self = shift;
    my($op, $cx) = @_;
    my $opname = $op->flags & OPf_SPECIAL ? 'CORE::require' : 'require';
    if (class($op) eq "UNOP" and $op->first->name eq "const"
	and $op->first->private & OPpCONST_BARE)
    {
	my $name = $self->const_sv($op->first)->PV;
	$name =~ s[/][::]g;
	$name =~ s/\.pm//g;
	return $self->maybe_parens("$opname $name", $cx, 16);
    } else {
	$self->unop(
	    $op, $cx,
	    $op->first->name eq 'const'
	     && $op->first->private & OPpCONST_NOVER
		 ? "no"
		 : $opname,
	    1, # llafr does not apply
	);
    }
}

sub pp_scalar
{
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first;
    if (not null $kid->sibling) {
	# XXX Was a here-doc
	return $self->dquote($op);
    }
    $self->unop(@_, "scalar");
}


sub padval
{
    my $self = shift;
    my $targ = shift;
    return $self->{'curcv'}->PADLIST->ARRAYelt(1)->ARRAYelt($targ);
}

sub anon_hash_or_list
{
    my $self = shift;
    my($op, $cx) = @_;

    my $name = $op->name;
    my($pre, $post) = @{{"anonlist" => ["[","]"],
			 "anonhash" => ["{","}"]}->{$name}};
    my($expr, @exprs);
    $op = $op->first->sibling; # skip pushmark
    for (; !null($op); $op = $op->sibling) {
	$expr = $self->deparse($op, 6, $op);
	push @exprs, $expr;
    }
    if ($pre eq "{" and $cx < 1) {
	# Disambiguate that it's not a block
	$pre = "+{";
    }
    my $texts = [$pre, join(", ", map($_->{text}, @exprs), $post)];
    return info_from_list($texts, '', $name, {body => \@exprs});
}

sub pp_anonlist {
    my $self = shift;
    my ($op, $cx) = @_;
    if ($op->flags & OPf_SPECIAL) {
	return $self->anon_hash_or_list($op, $cx);
    }
    warn "Unexpected op pp_" . $op->name() . " without OPf_SPECIAL";
    return info_from_text 'XXX', 'bad_anonlist', {};
}

*pp_anonhash = \&pp_anonlist;

sub e_anoncode($$)
{
    my ($self, $info) = @_;
    my $sub_info = $self->deparse_sub($info->{code});
    return info_from_list(['sub', $sub_info->{text}], ' ', 'e_anoncode',
	{body=> [$sub_info]});
}

sub pp_refgen
{
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first;
    if ($kid->name eq "null") {
	$kid = $kid->first;
	if (!null($kid->sibling) and
		 $kid->sibling->name eq "anoncode") {
            return $self->e_anoncode({ code => $self->padval($kid->sibling->targ) });
	} elsif ($kid->name eq "pushmark") {
            my $sib_name = $kid->sibling->name;
            if ($sib_name =~ /^(pad|rv2)[ah]v$/
                and not $kid->sibling->flags & OPf_REF)
            {
                # The @a in \(@a) isn't in ref context, but only when the
                # parens are there.
		my $list_info  = $self->pp_list($op->first);
		return info_from_list(['\(', $list_info->{text}, ')'], '',
				      'refgen_pushmark', {body=>$list_info});
            } elsif ($sib_name eq 'entersub') {
                my $kid_info = $self->deparse($kid->sibling, 1, $op);
                # Always show parens for \(&func()), but only with -p otherwise
		my @texts = ('\\', $kid_info->{text});
		if ($self->{'parens'} or $kid->sibling->private & OPpENTERSUB_AMPER) {
		    @texts = ('(', '\\', $kid_info->{text}, ')');
		}
		return info_from_list(\@texts, '', 'refgen_entersub',
		    {body => $kid_info});
            }
        }
    }
    $self->pfixop($op, $cx, "\\", 20);
}

sub pp_srefgen { pp_refgen(@_) }

sub pp_readline {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first;
    $kid = $kid->first if $kid->name eq "rv2gv"; # <$fh>
    if (is_scalar($kid)) {
	my $body = [$self->deparse($kid, 1, $op)];
	return info_from_list(['<', $body->[0]{text}, '>'], '',
			      'readline_scalar', {body=>$body});
    }
    return $self->unop($op, $cx, "readline");
}

sub pp_rcatline {
    my $self = shift;
    my($op) = @_;
    return info_from_list(["<", $self->gv_name($self->gv_or_padgv($op)), ">"],
			  '', 'rcatline', {});
}

# Unary operators that can occur as pseudo-listops inside double quotes
sub dq_unop {
    my $self = shift;
    my($op, $cx, $name, $prec, $flags) = (@_, 0, 0);
    my $kid;
    if ($op->flags & OPf_KIDS) {
       $kid = $op->first;
       # If there's more than one kid, the first is an ex-pushmark.
       $kid = $kid->sibling if not null $kid->sibling;
       return $self->maybe_parens_unop($name, $kid, $cx, $op);
    } else {
       return $name .  ($op->flags & OPf_SPECIAL ? "()" : "");
    }
}

sub pp_ucfirst { dq_unop(@_, "ucfirst") }
sub pp_lcfirst { dq_unop(@_, "lcfirst") }
sub pp_uc { dq_unop(@_, "uc") }
sub pp_lc { dq_unop(@_, "lc") }
sub pp_quotemeta { maybe_targmy(@_, \&dq_unop, "quotemeta") }
sub pp_fc { dq_unop(@_, "fc") }

# loop expressions
sub loopex
{
    my ($self, $op, $cx, $name) = @_;
    my $opts = {maybe_parens => [$self, $cx, 7]};
    my ($type, $body);
    if (class($op) eq "PVOP") {
	return info_from_list([$name, $op->pv], ' ', 'loopex_pvop', {});
    } elsif (class($op) eq "OP") {
	# no-op
	$type = 'loopex_op';
	return info_from_text($name, 'loopex_op', $opts);
    } elsif (class($op) eq "UNOP") {
	(my $kid_info = $self->deparse($op->first, 7, $op)) =~ s/^\cS//;
	$opts->{body} = [$kid_info];
	return info_from_list([$name, $op->pv], ' ', 'loopex_unop', $opts);
    } else {
	return info_from_text($name, 'loopex', $opts);
    }
    Carp::confess("unhandled condition in lopex");
}

sub pp_last { loopex(@_, "last") }
sub pp_next { loopex(@_, "next") }
sub pp_redo { loopex(@_, "redo") }
sub pp_goto { loopex(@_, "goto") }
sub pp_dump { loopex(@_, "CORE::dump") }

sub ftst {
    my $self = shift;
    my($op, $cx, $name) = @_;
    if (class($op) eq "UNOP") {
	# Genuine '-X' filetests are exempt from the LLAFR, but not
	# l?stat()
	if ($name =~ /^-/) {
	    (my $kid = $self->deparse($op->first, 16, $op)) =~ s/^\cS//;
	    return $self->maybe_parens("$name $kid", $cx, 16);
	}
	return $self->maybe_parens_unop($name, $op->first, $cx, $op);
    } elsif (class($op) =~ /^(SV|PAD)OP$/) {
	return $self->maybe_parens_func($name, $self->pp_gv($op, 1), $cx, 16);
    } else { # I don't think baseop filetests ever survive ck_ftst, but...
	return $name;
    }
}

sub pp_lstat    { ftst(@_, "lstat") }
sub pp_stat     { ftst(@_, "stat") }
sub pp_ftrread  { ftst(@_, "-R") }
sub pp_ftrwrite { ftst(@_, "-W") }
sub pp_ftrexec  { ftst(@_, "-X") }
sub pp_fteread  { ftst(@_, "-r") }
sub pp_ftewrite { ftst(@_, "-w") }
sub pp_fteexec  { ftst(@_, "-x") }
sub pp_ftis     { ftst(@_, "-e") }
sub pp_fteowned { ftst(@_, "-O") }
sub pp_ftrowned { ftst(@_, "-o") }
sub pp_ftzero   { ftst(@_, "-z") }
sub pp_ftsize   { ftst(@_, "-s") }
sub pp_ftmtime  { ftst(@_, "-M") }
sub pp_ftatime  { ftst(@_, "-A") }
sub pp_ftctime  { ftst(@_, "-C") }
sub pp_ftsock   { ftst(@_, "-S") }
sub pp_ftchr    { ftst(@_, "-c") }
sub pp_ftblk    { ftst(@_, "-b") }
sub pp_ftfile   { ftst(@_, "-f") }
sub pp_ftdir    { ftst(@_, "-d") }
sub pp_ftpipe   { ftst(@_, "-p") }
sub pp_ftlink   { ftst(@_, "-l") }
sub pp_ftsuid   { ftst(@_, "-u") }
sub pp_ftsgid   { ftst(@_, "-g") }
sub pp_ftsvtx   { ftst(@_, "-k") }
sub pp_fttty    { ftst(@_, "-t") }
sub pp_fttext   { ftst(@_, "-T") }
sub pp_ftbinary { ftst(@_, "-B") }

sub SWAP_CHILDREN () { 1 }
sub ASSIGN () { 2 } # has OP= variant
sub LIST_CONTEXT () { 4 } # Assignment is in list context

my(%left, %right);

sub assoc_class {
    my $op = shift;
    my $name = $op->name;
    if ($name eq "concat" and $op->first->name eq "concat") {
	# avoid spurious '=' -- see comment in pp_concat
	return "concat";
    }
    if ($name eq "null" and class($op) eq "UNOP"
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
    return $name . ($op->flags & OPf_STACKED ? "=" : "");
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

sub binop {
    my $self = shift;
    my ($op, $cx, $opname, $prec, $flags) = (@_, 0);
    my $left = $op->first;
    my $right = $op->last;
    my $eq = "";
    if ($op->flags & OPf_STACKED && $flags & ASSIGN) {
	$eq = "=";
	$prec = 7;
    }
    if ($flags & SWAP_CHILDREN) {
	($left, $right) = ($right, $left);
    }
    my $lhs = $self->deparse_binop_left($op, $left, $prec);
    if ($flags & LIST_CONTEXT
	&& $lhs->{text} !~ /^(my|our|local|)[\@\(]/) {
	$lhs->{text} = "($lhs->{text})";
    }
    my $rhs = $self->deparse_binop_right($op, $right, $prec);
    my @texts = ($lhs->{text}, "$opname$eq", $rhs->{text});
    my $text = $self->maybe_parens(join(' ', @texts), $cx, $prec);
    return {
	texts => \@texts,
	body => [$lhs, $rhs],
	text => $text,
    };
}

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

sub pp_eq { binop(@_, "==", 14) }
sub pp_ne { binop(@_, "!=", 14) }
sub pp_lt { binop(@_, "<", 15) }
sub pp_gt { binop(@_, ">", 15) }
sub pp_ge { binop(@_, ">=", 15) }
sub pp_le { binop(@_, "<=", 15) }
sub pp_ncmp { binop(@_, "<=>", 14) }
sub pp_i_eq { binop(@_, "==", 14) }
sub pp_i_ne { binop(@_, "!=", 14) }
sub pp_i_lt { binop(@_, "<", 15) }
sub pp_i_gt { binop(@_, ">", 15) }
sub pp_i_ge { binop(@_, ">=", 15) }
sub pp_i_le { binop(@_, "<=", 15) }
sub pp_i_ncmp { binop(@_, "<=>", 14) }

sub pp_seq { binop(@_, "eq", 14) }
sub pp_sne { binop(@_, "ne", 14) }
sub pp_slt { binop(@_, "lt", 15) }
sub pp_sgt { binop(@_, "gt", 15) }
sub pp_sge { binop(@_, "ge", 15) }
sub pp_sle { binop(@_, "le", 15) }
sub pp_scmp { binop(@_, "cmp", 14) }

sub pp_sassign { binop(@_, "=", 7, SWAP_CHILDREN) }
sub pp_aassign { binop(@_, "=", 7, SWAP_CHILDREN | LIST_CONTEXT) }

sub pp_smartmatch {
    my ($self, $op, $cx) = @_;
    if ($op->flags & OPf_SPECIAL) {
	return $self->deparse($op->last, $cx, $op);
    }
    else {
	binop(@_, "~~", 14);
    }
}

sub bin_info_join($$$$$) {
    my ($lhs, $rhs, $mid, $sep, $type) = @_;
    my $texts = [$lhs->{text}, $mid, $rhs->{text}];
    my $text = join(' ', @$texts);
    return {
	text => $text,
	body => [$lhs, $rhs],
	texts => $texts,
	type => $type
    };
}

sub bin_info_join_maybe_parens($$$$$$) {
    my ($self, $lhs, $rhs, $mid, $sep, $cx, $prec, $type) = @_;
    my $info = bin_info_join($lhs, $rhs, $mid, $sep, $type);
    $info->{text} = $self->maybe_parens($info->{text}, $cx, $prec);
    return $info;
}

# '.' is special because concats-of-concats are optimized to save copying
# by making all but the first concat stacked. The effect is as if the
# programmer had written '($a . $b) .= $c', except legal.
sub pp_concat { maybe_targmy(@_, \&real_concat) }
sub real_concat {
    my $self = shift;
    my($op, $cx) = @_;
    my $left = $op->first;
    my $right = $op->last;
    my $eq = "";
    my $prec = 18;
    if ($op->flags & OPf_STACKED and $op->first->name ne "concat") {
	$eq = "=";
	$prec = 7;
    }
    my $lhs = $self->deparse_binop_left($op, $left, $prec);
    my $rhs  = $self->deparse_binop_right($op, $right, $prec);
    return $self->bin_info_join_maybe_parens($lhs, $rhs, ".$eq", " ", $cx, $prec,
	'real_concat');
}

# 'x' is weird when the left arg is a list
sub pp_repeat {
    my $self = shift;
    my($op, $cx) = @_;
    my $left = $op->first;
    my $right = $op->last;
    my $eq = "";
    my $prec = 19;
    if ($op->flags & OPf_STACKED) {
	$eq = "=";
	$prec = 7;
    }
    my @exprs = ();
    my ($left_info, @body);
    if (null($right)) { # list repeat; count is inside left-side ex-list
	my $kid = $left->first->sibling; # skip pushmark
	for (; !null($kid->sibling); $kid = $kid->sibling) {
	    push @exprs, $self->deparse($kid, 6, $op);
	}
	$right = $kid;
	@body = @exprs;
	my $args = join(', ', map($_->{text}, @exprs));
	$left_info = info_from_list(["(", $args, ")"], '', 'repeat_left', {body => \@exprs});
    } else {
	$left_info = $self->deparse_binop_left($op, $left, $prec);
    }
    my $right_info  = $self->deparse_binop_right($op, $right, $prec);
    my $texts = [$left_info->{text}, "x$eq", $right_info->{text}];
    return info_from_list($texts, ' ', 'repeat',
			  {body => [$left_info, $right_info],
			   maybe_parens => [$self, $cx, $prec]});
}

sub range {
    my $self = shift;
    my ($op, $cx, $type) = @_;
    my $left = $op->first;
    my $right = $left->sibling;
    $left = $self->deparse($left, 9, $op);
    $right = $self->deparse($right, 9, $op);
    return info_from_list([$left, $type, $right], ' ', 'range',
			  {maybe_parens => [$self, $cx, 9]});
}

sub pp_flop {
    my $self = shift;
    my($op, $cx) = @_;
    my $flip = $op->first;
    my $type = ($flip->flags & OPf_SPECIAL) ? "..." : "..";
    return info_from_text $self->range($flip->first, $cx, $type), 'pp_flop', {};
}

# Logical ops, if/until, &&, and
# The one-line while/until is handled in pp_leave
sub logop
{
    my $self = shift;
    my ($op, $cx, $lowop, $lowprec, $highop, $highprec, $blockname) = @_;
    my $left = $op->first;
    my $right = $op->first->sibling;
    my ($lhs, $rhs, $texts, $text);
    if ($cx < 1 and is_scope($right) and $blockname
	and $self->{'expand'} < 7) {
	# if ($a) {$b}
	$lhs = $self->deparse($left, 1, $op);
	$rhs = $self->deparse($right, 0, $op);
	$texts = [$blockname, ' ', $lhs->{text}, ')', ' ',
		  "{\n\t", $rhs->{text}, "\n\b}\cK"];
	$text = join('', @$texts);
    } elsif ($cx < 1 and $blockname and not $self->{'parens'}
	     and $self->{'expand'} < 7) { # $b if $a
	$lhs = $self->deparse($left, 1, $op);
	$rhs = $self->deparse($right, 1, $op);
	$texts = [$rhs->{text}, $blockname, $lhs->{text}];
	$text = join(' ', @$texts);
    } elsif ($cx > $lowprec and $highop) {
	# $a && $b
	$lhs = $self->deparse_binop_left($op, $left, $highprec);
	$rhs = $self->deparse_binop_right($op, $right, $highprec);
	$texts = [$lhs->{text}, $highop, $rhs->{text}];
	$text = $self->maybe_parens(join(' ', @$texts), $cx, $highprec);
    } else {
	# $a and $b
	$lhs = $self->deparse_binop_left($op, $left, $lowprec);
	$rhs = $self->deparse_binop_right($op, $right, $lowprec);
	$texts = [$lhs->{text}, $lowop, $rhs->{text}];
	$text = $self->maybe_parens(join(' ', @$texts), $cx, $highprec);
    }
    return {
	block => [$lhs, $rhs],
	texts => $texts,
	text => $text,
    };
}

sub pp_and { logop(@_, "and", 3, "&&", 11, "if") }
sub pp_or  { logop(@_, "or",  2, "||", 10, "unless") }
sub pp_dor { logop(@_, "//", 10) }

# xor is syntactically a logop, but it's really a binop (contrary to
# old versions of opcode.pl). Syntax is what matters here.
sub pp_xor { logop(@_, "xor", 2, "",   0,  "") }

sub logassignop {
    my $self = shift;
    my ($op, $cx, $opname) = @_;
    my $left = $op->first;
    my $right = $op->first->sibling->first; # skip sassign
    $left = $self->deparse($left, 7);
    $right = $self->deparse($right, 7);
    return $self->maybe_parens("$left $opname $right", $cx, 7);
}

sub pp_andassign { logassignop(@_, "&&=") }
sub pp_orassign  { logassignop(@_, "||=") }
sub pp_dorassign { logassignop(@_, "//=") }

sub rv2gv_or_string {
    my($self,$op, $parent) = @_;
    if ($op->name eq "gv") { # could be open("open") or open("###")
	my($name,$quoted) =
	    $self->stash_variable_name("", $self->gv_or_padgv($op));
	return info_from_text($quoted ? $name : "*$name", 'r2gv_or_string', {});
    }
    else {
	return $self->deparse($op, 6, $parent);
    }
}

sub listop
{
    my($self, $op, $cx, $name, $kid, $nollafr) = @_;
    my(@exprs);
    my $parens = ($cx >= 5) || $self->{'parens'};
    my $other_ops = undef;

    unless ($kid) {
	$other_ops = [$op->first];
	$kid = $op->first->sibling;
    }

    # If there are no arguments, add final parentheses (or parenthesize the
    # whole thing if the llafr does not apply) to account for cases like
    # (return)+1 or setpgrp()+1.  When the llafr does not apply, we use a
    # precedence of 6 (< comma), as "return, 1" does not need parentheses.
    if (null $kid) {
	my $text = $nollafr
	    ? $self->maybe_parens($self->keyword($name), $cx, 7)
	    : $self->keyword($name) . '()' x (7 < $cx);
	return {text => $text};
    }
    my $first;
    my $fullname = $self->keyword($name);
    my $proto = prototype("CORE::$name");
    if (
	 (     (defined $proto && $proto =~ /^;?\*/)
	    || $name eq 'select' # select(F) doesn't have a proto
	 )
	 && $kid->name eq "rv2gv"
	 && !($kid->private & OPpLVAL_INTRO)
    ) {
	$first = $self->rv2gv_or_string($kid->first, $op);
    }
    else {
	$first = $self->deparse($kid, 6, $op);
    }
    if ($name eq "chmod" && $first->{text} =~ /^\d+$/) {
	$first = info_from_text sprintf("%#o", $first), 'listop_chmod', {};
    }
    $first->{text} = "+" + $first->{text}
	if not $parens and not $nollafr and substr($first->{text}, 0, 1) eq "(";
    push @exprs, $first;
    $kid = $kid->sibling;
    if (defined $proto && $proto =~ /^\*\*/ && $kid->name eq "rv2gv"
	&& !($kid->private & OPpLVAL_INTRO)) {
	$first = $self->rv2gv_or_string($kid->first, $op);
	push @exprs, $first;
	$kid = $kid->sibling;
    }
    for (; !null($kid); $kid = $kid->sibling) {
	push @exprs, $self->deparse($kid, 6, $op);
    }

    if ($name eq "reverse" && ($op->private & OPpREVERSE_INPLACE)) {
	my $texts =  [$exprs[0->{text}], '=',
		      $fullname . ($parens ? "($exprs[0]->{text})" : " $exprs[0]->{text}")];
	return {
	    texts => @$texts,
	    type => 'listop_reverse',
	    body => @exprs,
	    text => join(' ', @$texts),
	};
    }

    my $info = {
	body => \@exprs,
    };
    my @texts = map { $_->{text} } @exprs ;

    if ($name =~ /^(system|exec)$/
	&& ($op->flags & OPf_STACKED)
	&& @texts > 1)
    {
	# handle the "system prog a1, a2, ..." form
	my $prog = shift @texts;
	$texts[0] = "$prog $exprs[0]";
    }

    my $text;
    if ($parens && $nollafr) {
	$text = "($fullname " . join(", ", @texts) . ")";
    } elsif ($parens) {
	$text = "$fullname(" . join(", ", @texts) . ")";
    } else {
	$text = "$fullname " . join(", ", @texts);
    }
    unshift @texts, $fullname;
    $info->{texts} = \@texts;
    $info->{text} = $text;
    $info->{other_ops} = $other_ops if $other_ops;
    return $info;
}

sub pp_bless { listop(@_, "bless") }
sub pp_atan2 { maybe_targmy(@_, \&listop, "atan2") }
sub pp_substr {
    my ($self,$op,$cx) = @_;
    if ($op->private & OPpSUBSTR_REPL_FIRST) {
	return
	   listop($self, $op, 7, "substr", $op->first->sibling->sibling)
	 . " = "
	 . $self->deparse($op->first->sibling, 7);
    }
    maybe_local(@_, listop(@_, "substr"))
}
sub pp_vec { maybe_local(@_, listop(@_, "vec")) }
sub pp_index { maybe_targmy(@_, \&listop, "index") }
sub pp_rindex { maybe_targmy(@_, \&listop, "rindex") }
sub pp_sprintf { maybe_targmy(@_, \&listop, "sprintf") }
sub pp_formline { listop(@_, "formline") } # see also deparse_format
sub pp_crypt { maybe_targmy(@_, \&listop, "crypt") }
sub pp_unpack { listop(@_, "unpack") }
sub pp_pack { listop(@_, "pack") }
sub pp_join { maybe_targmy(@_, \&listop, "join") }
sub pp_splice { listop(@_, "splice") }
sub pp_push { maybe_targmy(@_, \&listop, "push") }
sub pp_unshift { maybe_targmy(@_, \&listop, "unshift") }
sub pp_reverse { listop(@_, "reverse") }
sub pp_warn { listop(@_, "warn") }
sub pp_die { listop(@_, "die") }
sub pp_return { listop(@_, "return", undef, 1) } # llafr does not apply
sub pp_open { listop(@_, "open") }
sub pp_pipe_op { listop(@_, "pipe") }
sub pp_tie { listop(@_, "tie") }
sub pp_binmode { listop(@_, "binmode") }
sub pp_dbmopen { listop(@_, "dbmopen") }
sub pp_sselect { listop(@_, "select") }
sub pp_select { listop(@_, "select") }
sub pp_read { listop(@_, "read") }
sub pp_sysopen { listop(@_, "sysopen") }
sub pp_sysseek { listop(@_, "sysseek") }
sub pp_sysread { listop(@_, "sysread") }
sub pp_syswrite { listop(@_, "syswrite") }
sub pp_send { listop(@_, "send") }
sub pp_recv { listop(@_, "recv") }
sub pp_seek { listop(@_, "seek") }
sub pp_fcntl { listop(@_, "fcntl") }
sub pp_ioctl { listop(@_, "ioctl") }
sub pp_flock { maybe_targmy(@_, \&listop, "flock") }
sub pp_socket { listop(@_, "socket") }
sub pp_sockpair { listop(@_, "socketpair") }
sub pp_bind { listop(@_, "bind") }
sub pp_connect { listop(@_, "connect") }
sub pp_listen { listop(@_, "listen") }
sub pp_accept { listop(@_, "accept") }
sub pp_shutdown { listop(@_, "shutdown") }
sub pp_gsockopt { listop(@_, "getsockopt") }
sub pp_ssockopt { listop(@_, "setsockopt") }
sub pp_chown { maybe_targmy(@_, \&listop, "chown") }
sub pp_unlink { maybe_targmy(@_, \&listop, "unlink") }
sub pp_chmod { maybe_targmy(@_, \&listop, "chmod") }
sub pp_utime { maybe_targmy(@_, \&listop, "utime") }
sub pp_rename { maybe_targmy(@_, \&listop, "rename") }
sub pp_link { maybe_targmy(@_, \&listop, "link") }
sub pp_symlink { maybe_targmy(@_, \&listop, "symlink") }
sub pp_mkdir { maybe_targmy(@_, \&listop, "mkdir") }
sub pp_open_dir { listop(@_, "opendir") }
sub pp_seekdir { listop(@_, "seekdir") }
sub pp_waitpid { maybe_targmy(@_, \&listop, "waitpid") }
sub pp_system { maybe_targmy(@_, \&listop, "system") }
sub pp_exec { maybe_targmy(@_, \&listop, "exec") }
sub pp_kill { maybe_targmy(@_, \&listop, "kill") }
sub pp_setpgrp { maybe_targmy(@_, \&listop, "setpgrp") }
sub pp_getpriority { maybe_targmy(@_, \&listop, "getpriority") }
sub pp_setpriority { maybe_targmy(@_, \&listop, "setpriority") }
sub pp_shmget { listop(@_, "shmget") }
sub pp_shmctl { listop(@_, "shmctl") }
sub pp_shmread { listop(@_, "shmread") }
sub pp_shmwrite { listop(@_, "shmwrite") }
sub pp_msgget { listop(@_, "msgget") }
sub pp_msgctl { listop(@_, "msgctl") }
sub pp_msgsnd { listop(@_, "msgsnd") }
sub pp_msgrcv { listop(@_, "msgrcv") }
sub pp_semget { listop(@_, "semget") }
sub pp_semctl { listop(@_, "semctl") }
sub pp_semop { listop(@_, "semop") }
sub pp_ghbyaddr { listop(@_, "gethostbyaddr") }
sub pp_gnbyaddr { listop(@_, "getnetbyaddr") }
sub pp_gpbynumber { listop(@_, "getprotobynumber") }
sub pp_gsbyname { listop(@_, "getservbyname") }
sub pp_gsbyport { listop(@_, "getservbyport") }
sub pp_syscall { listop(@_, "syscall") }

sub pp_glob {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first->sibling;  # skip pushmark
    my $keyword =
	$op->flags & OPf_SPECIAL ? 'glob' : $self->keyword('glob');
    my $text;
    if ($keyword =~ /^CORE::/
	or $kid->name ne 'const'
	or ($text = $self->dq($kid))
	     =~ /^\$?(\w|::|\`)+$/ # could look like a readline
        or $text =~ /[<>]/) {
	my $kid = $self->deparse($kid, 0, $op);
	if ($cx >= 5 || $self->{'parens'}) {
	    return info_from_list([$keyword, '(', $text, ')'], '',
			   'glob_paren', {});
	} else {
	    return info_from_list([$keyword, $text], ' ',
				  'glob_space', {});
	}
    } else {
	return info_from_list(['<', $text, '>'], '',
			      'glob_angle', {});
    }
}

# Truncate is special because OPf_SPECIAL makes a bareword first arg
# be a filehandle. This could probably be better fixed in the core
# by moving the GV lookup into ck_truc.

sub pp_truncate {
    my $self = shift;
    my($op, $cx) = @_;
    my(@exprs);
    my $parens = ($cx >= 5) || $self->{'parens'};
    my $kid = $op->first->sibling;
    my $fh;
    if ($op->flags & OPf_SPECIAL) {
	# $kid is an OP_CONST
	$fh = $self->const_sv($kid)->PV;
    } else {
	$fh = $self->deparse($kid, 6);
        $fh = "+$fh" if not $parens and substr($fh, 0, 1) eq "(";
    }
    my $len = $self->deparse($kid->sibling, 6);
    my $name = $self->keyword('truncate');
    if ($parens) {
	return "$name($fh, $len)";
    } else {
	return "$name $fh, $len";
    }
}

sub indirop {
    my $self = shift;
    my($op, $cx, $name) = @_;
    my($expr, @exprs);
    my $firstkid = my $kid = $op->first->sibling;
    my $indir_info;
    my @texts = ();
    my $type = 'indirop';

    if ($op->flags & OPf_STACKED) {
	my $indir_op = $kid->first; # skip rv2gv
	my $indir;
	if (is_scope($indir_op)) {
	    $indir_info = $self->deparse($indir_op, 0, $op);
	    @texts = $indir_info->{text} eq '' ?
		("{", ';', "}") : ("{", $indir_info->{texts}, "}");
	} elsif ($indir_op->name eq "const" && $indir_op->private & OPpCONST_BARE) {
	    @texts = ($self->const_sv($indir_op)->PV);
	} else {
	    $indir_info = $self->deparse($indir_op, 24, $op);
	    @texts = @{$indir_info->{texts}};
	}
	push @texts, '';  # To get a space in between this and the next
	$kid = $kid->sibling;
    }
    if ($name eq "sort" && $op->private & (OPpSORT_NUMERIC | OPpSORT_INTEGER)) {
	$type = 'sort_num_int';
	@texts = ($op->private & OPpSORT_DESCEND) ?
	    ('{', $b, '<=>', '$a', '}' ) : ('{', $a, '<=>', '$b', '}' );
    }
    elsif ($name eq "sort" && $op->private & OPpSORT_DESCEND) {
	$type = 'sort_descend';
	@texts =  ('{', $b, 'cmp', '}' );
    }

    my $indir = join(' ', @texts);
    for (; !null($kid); $kid = $kid->sibling) {
	my $cx = (!$indir &&
		  $kid == $firstkid && $name eq "sort" &&
		  $firstkid->name eq "entersub")
	    ? 16 : 6;
	$expr = $self->deparse($kid, $cx, $op);
	push @exprs, $expr;
    }
    my @texts2;
    if ($name eq "sort" && $op->private & OPpSORT_REVERSE) {
	$type = 'sort_reverse';
	@texts2 = ($self->keyword('reverse'), $self->keyword('sort'));
    }  else {
	@texts2 = ( $self->keyword($name) );
    }
    if ($name eq "sort" && ($op->private & OPpSORT_INPLACE)) {
	@texts = ($exprs[0]->{text}, '=', @texts2, @texts, $exprs[0]->{text});
	my $text = join(' ', @texts);
	return {
	    text => $text,
	    texts => \@texts,
	    type => 'sort_inplace',
	}
    }

    my @body = @exprs;
    unshift @body, $indir_info if defined($indir_info);

    my $args = $indir .  join(', ', map($_->{text},  @exprs));
    if (0 == scalar @texts && $name eq "sort") {
	# We don't want to say "sort(f 1, 2, 3)", since perl -w will
	# give bareword warnings in that case. Therefore if context
	# requires, we'll put parens around the outside "(sort f 1, 2,
	# 3)". Unfortunately, we'll currently think the parens are
	# necessary more often that they really are, because we don't
	# distinguish which side of an assignment we're on.
	if ($cx >= 5) {
	    @texts = ('(', @texts2, $args, ')');
	} else {
	    @texts = (@texts2, $args);
	}
	my $text = join(' ', @texts);
	return {
	    text => $text,
	    texts => \@texts,
	    body => \@body,
	    type => 'sort1',
	}
    } elsif (0 != scalar @texts
	     && $name eq "sort"
	     && !null($op->first->sibling)
	     && $op->first->sibling->name eq 'entersub' ) {
	# We cannot say sort foo(bar), as foo will be interpreted as a
	# comparison routine.  We have to say sort(...) in that case.
	@texts = (@texts2, '(', $args, ')');
	my $text = join(' ', @texts);
	return {
	    text => $text,
	    texts => \@texts,
	    body => \@body,
	    type => 'sort2',
	}
    } else {
	@texts = $self->maybe_parens_func($texts2[0], $args, $cx, 5);
	$args = join('', @texts);
	return {
	    text => $args,
	    texts => \@texts,
	    body => \@body,
	    type => $type,
	}
    }

}

sub pp_prtf { indirop(@_, "printf") }
sub pp_print { indirop(@_, "print") }
sub pp_say  { indirop(@_, "say") }
sub pp_sort { indirop(@_, "sort") }

sub mapop {
    my $self = shift;
    my($op, $cx, $name) = @_;
    my($expr, @exprs);
    my $kid = $op->first; # this is the (map|grep)start
    $kid = $kid->first->sibling; # skip a pushmark
    my $code = $kid->first; # skip a null
    if (is_scope $code) {
	$code = "{" . $self->deparse($code, 0) . "} ";
    } else {
	$code = $self->deparse($code, 24);
	$code .= ", " if !null($kid->sibling);
    }
    $kid = $kid->sibling;
    for (; !null($kid); $kid = $kid->sibling) {
	$expr = $self->deparse($kid, 6);
	push @exprs, $expr if defined $expr;
    }
    return $self->maybe_parens_func($name, $code . join(", ", @exprs), $cx, 5);
}

sub pp_mapwhile { mapop(@_, "map") }
sub pp_grepwhile { mapop(@_, "grep") }
sub pp_mapstart { baseop(@_, "map") }
sub pp_grepstart { baseop(@_, "grep") }

sub pp_list {
    my $self = shift;
    my($op, $cx) = @_;
    my($expr, @exprs);

    my $other_ops = [$op->first];
    my $kid = $op->first->sibling; # skip pushmark

    if (class($kid) eq 'NULL') {
	return info_from_text('', 'list_null',
			      {other_ops => $other_ops});
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

	if (!($lop->private & (OPpLVAL_INTRO|OPpOUR_INTRO)
		or $lop->name eq "undef")
	    or $lop->name =~ /^(?:entersub|exit|open|split)\z/)
	{
	    $local = ""; # or not
	    last;
	}
	if ($lop->name =~ /^pad[ash]v$/) {
	    if ($lop->private & OPpPAD_STATE) { # state()
		($local = "", last) if $local =~ /^(?:local|our|my)$/;
		$local = "state";
	    } else { # my()
		($local = "", last) if $local =~ /^(?:local|our|state)$/;
		$local = "my";
	    }
	} elsif ($lop->name =~ /^(gv|rv2)[ash]v$/
			&& $lop->private & OPpOUR_INTRO
		or $lop->name eq "null" && $lop->first->name eq "gvsv"
			&& $lop->first->private & OPpOUR_INTRO) { # our()
	    ($local = "", last) if $local =~ /^(?:my|local|state)$/;
	    $local = "our";
	} elsif ($lop->name ne "undef"
		# specifically avoid the "reverse sort" optimisation,
		# where "reverse" is nullified
		&& !($lop->name eq 'sort' && ($lop->flags & OPpSORT_REVERSE)))
	{
	    # local()
	    ($local = "", last) if $local =~ /^(?:my|our|state)$/;
	    $local = "local";
	}
    }
    $local = "" if $local eq "either"; # no point if it's all undefs
    if (null $kid->sibling and not $local) {
	my $info = $self->deparse($kid, $cx, $op);
	$info->{other_ops} = $other_ops;
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
    my @args = map $_->{text}, @exprs;
    my $info = {
	body => \@exprs,
    };

    my @texts = ($local, '(', join(", ", @args), ')');
    if ($local) {
	$info->{texts} = \@texts;
	$info->{text} = join('', @texts);
	$info->{type} = 'pp_list_local';
    } else {
	$info->{texts} = @args;
	$info->{text} = $self->maybe_parens(join(", ", @args), $cx, 6);
	$info->{type} = 'pp_list';
    }
    $info->{other_ops} = $other_ops;
    return $info;
}

sub is_ifelse_cont
{
    my $op = shift;
    return ($op->name eq "null" and class($op) eq "UNOP"
	    and $op->first->name =~ /^(and|cond_expr)$/
	    and is_scope($op->first->first->sibling));
}

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
	my $text = $self->maybe_parens(join(' ', @texts), $cx, 8);
	return {
	    text => $text,
	    texts => \@texts,
	    type => 'pp_cond_expr1',
	    body => [$cond_info, $true_info, $false_info],
	};
    }

    my $cond_info = $self->deparse($cond, 1, $op);
    my $true_info = $self->deparse($true, 0, $op);
    my @head = ('if ', '(', $cond_info->{text}, ') ', "{\n\t", $true_info->{text}, "\n\b}");
    my @elsifs;
    my @body = ($cond_info, $true_info);

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
	push @body, $newcond_info;
	push @body, $newtrue_info;
	push @elsifs, "elsif ($newcond_info->{text}) {\n\t$newtrue_info->{text}\n\b}";
    }
    my $false_info;
    if (!null($false)) {
	$false_info = $self->deparse($false, 0, $op);
	$false_info->{text} = $cuddle . "else {\n\t" . $false_info->{text} . "\n\b}\cK";
	push @body, $false_info;
    } else {
	$false_info->{text} = "\cK";
    }
    my @texts = (@head, @elsifs, $false_info->{text});
    my $text = join('', @head) . join($cuddle, @elsifs) . $false_info->{text};
    return {
	text => $text,
	texts => \@texts,
	body => \@body,
	type => 'pp_cond_expr',
    };
}

sub pp_once
{
    my ($self, $op, $cx) = @_;
    my $cond = $op->first;
    my $true = $cond->sibling;

    return $self->deparse($true, $cx);
}

sub loop_common
{
    my $self = shift;
    my($op, $cx, $init) = @_;
    my $enter = $op->first;
    my $kid = $enter->sibling;
    local(@$self{qw'curstash warnings hints hinthash'})
		= @$self{qw'curstash warnings hints hinthash'};

    my ($body, @body, $type);
    my (@head, @head_text) = ((), ());
    my ($bare, $cond_info) = (0, undef);

    if ($kid->name eq "lineseq") {
	# bare or infinite loop
	$type = 'loop_bare_or_infinite';

	if ($kid->last->name eq "unstack") { # infinite
	    @head_text = ("while", "(1)"); # Can't use for(;;) if there's a continue
	} else {
	    $bare = 1;
	}
	$body = $kid;
    } elsif ($enter->name eq "enteriter") {
	# foreach
	$type = 'loop_foreach';

	my $ary = $enter->first->sibling; # first was pushmark
	my @ary_info;
	my @ary_text;
	my $var = $ary->sibling;
	if ($ary->name eq 'null' and $enter->private & OPpITER_REVERSED) {
	    # "reverse" was optimised away
	    @ary_info = (listop($self, $ary->first->sibling, 1, 'reverse'));
	    @ary_text = ($ary_info[0]->{text});
	} elsif ($enter->flags & OPf_STACKED
		 and not null $ary->first->sibling->sibling) {
	    @ary_info = ($self->deparse($ary->first->sibling, 9, $op),
			 $self->deparse($ary->first->sibling->sibling, 9, $op));
	    @ary_text = ($ary_info[0]->{text}, "..", $ary_info[1]->{text});

	} else {
	    @ary_info = ($self->deparse($ary, 1, $op));
	    @ary_text = $ary_info[0]->{text};
	}

	my @var_info = ();
	my @var_text;
	if (null $var) {
	    if (($enter->flags & OPf_SPECIAL) && ($] < 5.009)) {
		# thread special var, under 5005threads
		@var_text = ($self->pp_threadsv($enter, 1));
	    } else { # regular my() variable
		@var_text = ($self->pp_padsv($enter, 1, 1));
	    }
	} elsif ($var->name eq "rv2gv") {
	    @var_info = ($self->pp_rv2sv($var, 1));
	    @var_text = ($var_info[0]->{text});
	    if ($enter->private & OPpOUR_INTRO) {
		# our declarations don't have package names
		$var_text[0] =~ s/^(.).*::/$1/;
		unshift @var_text, 'our';
	    }
	} elsif ($var->name eq "gv") {
	    @var_info = ($self->deparse($var, 1, $op));
	    @var_text = ("\$" . $var_info[0]->{text});
	}

	# skip OP_AND and OP_ITER
	my $other_ops = [$kid->first, $kid->first->first];
	$body = $kid->first->first->sibling;

	@head = (@ary_info, @var_info);
	if (!is_state $body->first and $body->first->name !~ /^(?:stub|leave|scope)$/) {
	    Carp::confess("var ne \$_") unless join('', @var_text) eq '$_';
	    $body = $body->first;
	    my $body_info = $self->deparse($body, 2, $op);
	    push @head, $body_info;
	    my @texts = ($body_info->{text}, "foreach", '(', @ary_text, ')');
	    return info_from_list(\@texts, ' ', 'loop_foreach_ary',
				  {body => \@head, other_ops => $other_ops});
	}
	@head_text = ("foreach", @var_text, '(', @ary_text, ')');
    } elsif ($kid->name eq "null") {
	# while/until
	$type = 'loop_while_until';

	$kid = $kid->first;
	my $name = {"and" => "while", "or" => "until"}->{$kid->name};
	$cond_info = $self->deparse($kid->first, 1, $op);
	@head_text = ($name,  "($cond_info->{text})");
	$body = $kid->first->sibling;
    } elsif ($kid->name eq "stub") {
	# bare and empty
	return info_from_list(['{', ';', '}'], '', 'loop_stub', {});
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
	$type .= '_continue';

	if ($bare) {
	    $cont = $body->last;
	} else {
	    $cont = $body->first;
	    while (!null($cont->sibling->sibling)) {
		$cont = $cont->sibling;
	    }
	}
	my $state = $body->first;
	my $cuddle = $self->{'cuddle'};
	my @states;
	for (; $$state != $$cont; $state = $state->sibling) {
	    push @states, $state;
	}
	$body_info = $self->lineseq(undef, 0, @states);
	if (defined $cond_info and not is_scope $cont and $self->{'expand'} < 3) {
	    my $cont_info = $self->deparse($cont, 1, $op);
	    push @head, $cont_info;
	    my $init_text = defined($init) ? $init->{text} : ' ';
	    @head_text = ('for', '(', "$init_text;", $cont_info->{text}, ')');
	    @cont_text = ("\cK");
	} else {
	    my $cont_info = $self->deparse($cont, 0, $op);
	    push @head, $cont_info;
	    @cont_text = ($cuddle, 'continue', "{\n\t",
			  $cont_info->{text} , "\n\b}\cK");
	}
    } else {
	return info_from_text([''], 'loop_no_body', {}) if !defined $body;
	if (defined $init) {
	    @head_text = ('for', '(', "$init->{text};", "$cond_info->{text};", ")");
	}
	@cont_text = ("\cK");
	$body_info = $self->deparse($body, 0, $op);
    }
    (my $body_text = $body_info->{text}) =~ s/;?$/;\n/;

    my @texts = (@head_text, "{\n\t", $body_text, "\b}", @cont_text);
    return info_from_list(\@texts, ' ', $type,
	{body => [$body_info]});
}

sub pp_leaveloop {
    shift->loop_common(@_, undef)
}

sub for_loop {
    my $self = shift;
    my($op, $cx, $parent) = @_;
    my $init = $self->deparse($op, 1, $parent);
    my $s = $op->sibling;
    my $ll = $s->name eq "unstack" ? $s->sibling : $s->first->sibling;
    return $self->loop_common($ll, $cx, $init);
}

sub pp_leavetry {
    my $self = shift;
    my $leave_info = $self->pp_leave(@_);
    return info_from_list(['eval', '{\n\t"', $leave_info->{text}, "\n\b}"],
			  ' ', 'leavetry', {body=>[$leave_info]});
}

sub _op_is_or_was {
  my ($op, $expect_type) = @_;
  my $type = $op->type;
  return($type == $expect_type
         || ($type == OP_NULL && $op->targ == $expect_type));
}

sub pp_null {
    my $self = shift;
    my($op, $cx) = @_;

    my $info;
    if (class($op) eq "OP") {
	# old value is lost
	if ($op->targ == OP_CONST) {
	    return info_from_text($self->{'ex_const'}, 'null_const', {})
	} else {
	    return info_from_text('???', 'null_unknown', {});
	}
    }
    my $kid = $op->first;
    if ($op->first->name eq 'pushmark'
             or $op->first->name eq 'null'
                && $op->first->targ == OP_PUSHMARK
                && _op_is_or_was($op, OP_LIST)) {
	return $self->pp_list($op, $cx);
    } elsif ($kid->name eq "enter") {
	return $self->pp_leave($op, $cx);
    } elsif ($kid->name eq "leave") {
	return $self->pp_leave($kid, $cx);
    } elsif ($kid->name eq "scope") {
	return $self->pp_scope($kid, $cx);
    } elsif ($op->targ == OP_STRINGIFY) {
	return $self->dquote($op, $cx);
    } elsif ($op->targ == OP_GLOB) {
	return $self->pp_glob(
	         $kid    # entersub
	            ->first    # ex-list
	            ->first    # pushmark
	            ->sibling, # glob
	         $cx
	       );
    } elsif (!null($kid->sibling) and
    	     $kid->sibling->name eq "readline" and
    	     $kid->sibling->flags & OPf_STACKED) {
    	my $lhs = $self->deparse($kid, 7, $op);
    	my $rhs = $self->deparse($kid->sibling, 7, $kid);
    	return $self->bin_info_join_maybe_parens($lhs, $rhs, '=', " ", $cx, 7);
    } elsif (!null($kid->sibling) and
    	     $kid->sibling->name eq "trans" and
    	     $kid->sibling->flags & OPf_STACKED) {
    	my $lhs = $self->deparse($kid, 20, $op);
    	my $rhs = $self->deparse($kid->sibling, 20, $op);
    	return $self->bin_info_join_maybe_parens($lhs, $rhs, '=~', " ", $cx, 20);
    } elsif ($op->flags & OPf_SPECIAL && $cx < 1 && !$op->targ) {
    	my $kid_info = $self->deparse($kid, $cx, $op);
	return info_from_list(['do', "{\n\t", $kid_info->{text},
			       "\n\b};"], '', 'null_special',
	    {body => [$kid_info]});
    } elsif (!null($kid->sibling) and
	     $kid->sibling->name eq "null" and
	     class($kid->sibling) eq "UNOP" and
	     $kid->sibling->first->flags & OPf_STACKED and
	     $kid->sibling->first->name eq "rcatline") {
	my $lhs = $self->deparse($kid, 18);
	my $rhs = $self->deparse($kid->sibling, 18);
	return $self->bin_info_join_maybe_parens($lhs, $rhs, '=', " ", $cx, 20,
						 'null_rcatline');
    } else {
	return $self->deparse($kid, $cx, $op);
    }
    Carp::confess("unhandled condition in null");
}

sub padname {
    my $self = shift;
    my $targ = shift;
    return $self->padname_sv($targ)->PVX;
}

sub padany {
    my $self = shift;
    my $op = shift;
    return substr($self->padname($op->targ), 1); # skip $/@/%
}

sub pp_padsv {
    my $self = shift;
    my($op, $cx, $forbid_parens) = @_;
    return $self->maybe_my($op, $cx, $self->padname($op->targ),
			   $forbid_parens);
}

sub pp_padav { pp_padsv(@_) }
sub pp_padhv { pp_padsv(@_) }

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

sub pp_gvsv
{
    my($self, $op, $cx) = @_;
    my $gv = $self->gv_or_padgv($op);
    return $self->maybe_local_str($op, $cx,
				  $self->stash_variable("\$",
							$self->gv_name($gv), $cx));
}

sub pp_gv
{
    my($self, $op, $cx) = @_;
    my $gv = $self->gv_or_padgv($op);
    return info_from_text($self->gv_name($gv), 'pp_gv', {});
}

sub pp_aelemfast_lex
{
    my($self, $op, $cx) = @_;
    my $name = $self->padname($op->targ);
    $name =~ s/^@/\$/;
    return info_from_list([$name, "[", ($op->private + $self->{'arybase'}), "]"],
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
    return info_from_list([$name, "[", ($op->private + $self->{'arybase'}), "]"],
		      '', 'pp_aelemfast', {});
}

sub rv2x
{
    my($self, $op, $cx, $type) = @_;

    if (class($op) eq 'NULL' || !$op->can("first")) {
	carp("Unexpected op in pp_rv2x");
	return info_from_text 'XXX', 'bad_rv2x', {};
    }
    my ($info, $kid_info);
    my $kid = $op->first;
    if ($kid->name eq "gv") {
	$kid_info = $self->deparse($kid, 0, $op);
	my $str = $self->stash_variable($type, $kid_info->{text}, $cx);
	return info_from_text($str, 'rv2x_gv', {body => [$kid_info]});
    } elsif (is_scalar $kid) {
	$kid_info = $self->deparse($kid, 0, $op);
	my $str = $kid_info->{text};
	if ($str =~ /^\$([^\w\d])\z/) {
	    # "$$+" isn't a legal way to write the scalar dereference
	    # of $+, since the lexer can't tell you aren't trying to
	    # do something like "$$ + 1" to get one more than your
	    # PID. Either "${$+}" or "$${+}" are workable
	    # disambiguations, but if the programmer did the former,
	    # they'd be in the "else" clause below rather than here.
	    # It's not clear if this should somehow be unified with
	    # the code in dq and re_dq that also adds lexer
	    # disambiguation braces.
	    $str = '$' . "{$1}"; #'
	}
	return info_from_list([$type, $str], '', 'rv2x_scalar',
			      {body => [$kid_info]});
    } else {
	my $kid_info = $self->deparse($kid, 0, $op);
	return info_from_list([$type, "{", "}"], '', 'rv2x',
			      {body => [$kid_info]});
    }
    Carp::confess("unhandled condition in rv2x");
}

sub pp_rv2sv { maybe_local(@_, rv2x(@_, "\$")) }
sub pp_rv2hv { maybe_local(@_, rv2x(@_, "%")) }
sub pp_rv2gv { maybe_local(@_, rv2x(@_, "*")) }

# skip rv2av
sub pp_av2arylen {
    my $self = shift;
    my($op, $cx) = @_;
    if ($op->first->name eq "padav") {
	return $self->maybe_local_str($op, $cx, '$#' . $self->padany($op->first));
    } else {
	return $self->maybe_local($op, $cx,
				  $self->rv2x($op->first, $cx, '$#'));
    }
}

# skip down to the old, ex-rv2cv
sub pp_rv2cv {
    my ($self, $op, $cx) = @_;
    if (!null($op->first) && $op->first->name eq 'null' &&
	$op->first->targ == OP_LIST)
    {
	return $self->rv2x($op->first->first->sibling, $cx, "&")
    }
    else {
	return $self->rv2x($op, $cx, "")
    }
}

sub list_const {
    my $self = shift;
    my($cx, @list) = @_;
    my @a = map $self->const($_, 6), @list;
    my @texts = map $_->{text}, @a;
    my $type = 'list_const';
    my $prec = 6;
    if (@texts == 0) {
	return info_from_list(['(', ')'], '', 'list_const_null', {});
    } elsif (@texts == 1) {
	return info_from_text($texts[0], 'list_const_one',
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
    return info_from_list(\@texts,  '', $type,
	{maybe_parens => [$self, $cx, $prec]});
}

sub pp_rv2av {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first;
    if ($kid->name eq "const") { # constant list
	my $av = $self->const_sv($kid);
	return $self->list_const($cx, $av->ARRAY);
    } else {
	return $self->maybe_local($op, $cx, $self->rv2x($op, $cx, "\@"));
    }
 }

sub is_subscriptable {
    my $op = shift;
    if ($op->name =~ /^[ahg]elem/) {
	return 1;
    } elsif ($op->name eq "entersub") {
	my $kid = $op->first;
	return 0 unless null $kid->sibling;
	$kid = $kid->first;
	$kid = $kid->sibling until null $kid->sibling;
	return 0 if is_scope($kid);
	$kid = $kid->first;
	return 0 if $kid->name eq "gv" || $kid->name eq "padcv";
	return 0 if is_scalar($kid);
	return is_subscriptable($kid);
    } else {
	return 0;
    }
}

sub elem_or_slice_array_name
{
    my $self = shift;
    my ($array, $left, $padname, $allow_arrow) = @_;

    if ($array->name eq $padname) {
	return $self->padany($array);
    } elsif (is_scope($array)) { # ${expr}[0]
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
    } elsif (!$allow_arrow || is_scalar $array) { # $x[0], $$x[0], ...
	return $self->deparse($array, 24);
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
    $idx =~ s/^\((.*)\)$/$1/ if $self->{'parens'};

    # Hash-element braces will autoquote a bareword inside themselves.
    # We need to make sure that C<$hash{warn()}> doesn't come out as
    # C<$hash{warn}>, which has a quite different meaning. Currently
    # B::Deparse will always quote strings, even if the string was a
    # bareword in the original (i.e. the OPpCONST_BARE flag is ignored
    # for constant strings.) So we can cheat slightly here - if we see
    # a bareword, we know that it is supposed to be a function call.
    #
    $idx =~ s/^([A-Za-z_]\w*)$/$1()/;

    return info_from_text($idx, 'elem_or_slice_single_index',
			  {body => [$idx_info]});
}

sub elem
{
    my ($self, $op, $cx, $left, $right, $padname) = @_;
    my($array, $idx) = ($op->first, $op->first->sibling);

    my $idx_info = $self->elem_or_slice_single_index($idx, $op);
    my $opts = {body => [$idx_info]};

    unless ($array->name eq $padname) { # Maybe this has been fixed
	$array = $array->first; # skip rv2av (or ex-rv2av in _53+)
    }
    my @texts;
    my $info;
    my $array_name=$self->elem_or_slice_array_name($array, $left, $padname, 1);
    if ($array_name) {
	if ($array_name =~ /->\z/) {
	    @texts = ($array_name);
	} elsif ($array_name eq '#') {
	    @texts = ('${#}')
	}  else {
	    unshift @texts, '$' ;
	}
	push @texts, $left, $idx_info->{text}, $right;
	return info_from_list(\@texts, '', 'elem', $opts)
    } else {
	# $x[20][3]{hi} or expr->[20]
	my $arrow = is_subscriptable($array) ? "" : "->";
	my $info = $self->deparse($array, 24, $op);
	@texts = ($array_name , $arrow, $left, $idx, $right);
	info_from_list(\@texts, '', 'elem_arrow', $opts);
    }
    Carp::confess("unhandled condition in elem");
}

sub pp_aelem { maybe_local(@_, elem(@_, "[", "]", "padav")) }
sub pp_helem { maybe_local(@_, elem(@_, "{", "}", "padhv")) }

sub pp_gelem {
    my($self, $op, $cx) = @_;
    my($glob, $part) = ($op->first, $op->last);
    $glob = $glob->first; # skip rv2gv
    $glob = $glob->first if $glob->name eq "rv2gv"; # this one's a bug
    my $scope = is_scope($glob);
    $glob = $self->deparse($glob, 0);
    $part = $self->deparse($part, 1);
    return "*" . ($scope ? "{$glob}" : $glob) . "{$part}";
}

sub slice {
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
	# skip list, pushmark
	$kid = $kid->first->sibling;
	for (; !null $kid; $kid = $kid->sibling) {
	    push @elems, $self->deparse($kid, 6, $op);
	}
    } else {
	@elems = ($self->elem_or_slice_single_index($kid, $op));
    }
    my $list = join(', ', map($_->{text}, @elems));
    my $lead = '@';
    $lead = '%' if $op->name =~ /^kv/i;
    return info_from_list([$lead, $array_info->{text}, $left, $list, $right], '', 'slice',
	{body => \@elems});
}

sub pp_aslice { maybe_local(@_, slice(@_, "[", "]", "rv2av", "padav")) }
sub pp_hslice { maybe_local(@_, slice(@_, "{", "}", "rv2hv", "padhv")) }
sub pp_kvaslice {                 slice(@_, "[", "]", "rv2av", "padav")  }
sub pp_hslice   { maybe_local(@_, slice(@_, "{", "}", "rv2hv", "padhv")) }
sub pp_kvhslice {                 slice(@_, "{", "}", "rv2hv", "padhv")  }

sub pp_lslice {
    my $self = shift;
    my($op, $cx) = @_;
    my $idx = $op->first;
    my $list = $op->last;
    my(@elems, $kid);
    $list = $self->deparse($list, 1);
    $idx = $self->deparse($idx, 1);
    return "($list)" . "[$idx]";
}

sub want_scalar {
    my $op = shift;
    return ($op->flags & OPf_WANT) == OPf_WANT_SCALAR;
}

sub want_list {
    my $op = shift;
    return ($op->flags & OPf_WANT) == OPf_WANT_LIST;
}

sub _method {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first->sibling; # skip pushmark
    my($meth, $obj, @exprs);
    if ($kid->name eq "list" and want_list $kid) {
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
	$meth = $self->const_sv($meth)->PV;
    } else {
	$meth = $meth->first;
	if ($meth->name eq "const") {
	    # As of 5.005_58, this case is probably obsoleted by the
	    # method_named case above
	    $meth = $self->const_sv($meth)->PV; # needs to be bare
	}
    }

    return { method => $meth, variable_method => ref($meth),
             object => $obj, args => \@exprs  },
	   $cx;
}

# compat function only
sub method {
    my $self = shift;
    my $info = $self->_method(@_);
    return $self->e_method( $self->_method(@_) );
}

sub e_method {
    my ($self, $info, $cx) = @_;
    my $obj = $self->deparse($info->{object}, 24);

    my $meth = $info->{method};
    $meth = $self->deparse($meth, 1) if $info->{variable_method};
    my $args = join(", ", map { $self->deparse($_, 6) } @{$info->{args}} );
    if ($info->{object}->name eq 'scope' && want_list $info->{object}) {
	# method { $object }
	# This must be deparsed this way to preserve list context
	# of $object.
	my $need_paren = $cx >= 6;
	return '(' x $need_paren
	     . $meth . substr($obj,2) # chop off the "do"
	     . " $args"
	     . ')' x $need_paren;
    }
    my $kid = $obj . "->" . $meth;
    if (length $args) {
	return $kid . "(" . $args . ")"; # parens mandatory
    } else {
	return $kid;
    }
}

# returns "&" if the prototype doesn't match the args,
# or ("", $args_after_prototype_demunging) if it does.
sub check_proto {
    my $self = shift;
    my $op = shift;
    return "&" if $self->{'noproto'};
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
	    return "&" if @args;
	} elsif ($chr eq ";") {
	    $doneok = 1;
	} elsif ($chr eq "@" or $chr eq "%") {
	    push @reals, map($self->deparse($_, 6), @args, $op);
	    @args = ();
	} else {
	    $arg = shift @args;
	    last unless $arg;
	    if ($chr eq "\$" || $chr eq "_") {
		if (want_scalar $arg) {
		    push @reals, $self->deparse($arg, 6, $op);
		} else {
		    return "&";
		}
	    } elsif ($chr eq "&") {
		if ($arg->name =~ /^(s?refgen|undef)$/) {
		    push @reals, $self->deparse($arg, 6, $op);
		} else {
		    return "&";
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
		      return "&";
		  }
	    } elsif (substr($chr, 0, 1) eq "\\") {
		$chr =~ tr/\\[]//d;
		if ($arg->name =~ /^s?refgen$/ and
		    !null($real = $arg->first) and
		    ($chr =~ /\$/ && is_scalar($real->first)
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
		      return "&";
		  }
	    }
       }
    }
    return ('&', []) if $proto and !$doneok; # too few args and no ';'
    return ('&', []) if @args;               # too many args
    return ('', \@reals);
}

sub pp_entersub
{
    my($self, $op, $cx) = @_;
    return $self->e_method($self->_method($op, $cx))
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

    my $other_ops = [$kid->first];
    $kid = $kid->first->sibling; # skip ex-list, pushmark

    for (; not null $kid->sibling; $kid = $kid->sibling) {
	push @exprs, $kid;
    }
    my ($simple, $proto, $kid_info) = (0, undef, undef);
    if (is_scope($kid)) {
	$amper = "&";
	$kid_info = $self->deparse($kid, 0, $op);
	$kid_info->{texts} = ['{', $kid_info->texts, '}'];
	$kid_info->{text} = join('', @$kid_info->{texts});
    } elsif ($kid->first->name eq "gv") {
	my $gv = $self->gv_or_padgv($kid->first);
	if (class($gv->CV) ne "SPECIAL") {
	    $proto = $gv->CV->PV if $gv->CV->FLAGS & SVf_POK;
	}
	$simple = 1; # only calls of named functions can be prototyped
	$kid_info = $self->deparse($kid, 24, $op);
	if (!$amper) {
	    if ($kid_info->{text} eq 'main::') {
		$kid_info->{text} = '::';
	    } elsif ($kid_info->{text} !~ /^(?:\w|::)(?:[\w\d]|::(?!\z))*\z/) {
		$kid_info->{text} = single_delim("q", "'", $kid) . '->';
	    }
	}
    } elsif (is_scalar ($kid->first) && $kid->first->name ne 'rv2cv') {
	$amper = "&";
	$kid_info = $self->deparse($kid, 24, $op);
    } else {
	$prefix = "";
	my $arrow = is_subscriptable($kid->first) || $kid->first->name eq "padcv" ? "" : "->";
	$kid_info = $self->deparse($kid, 24, $op);
	$kid_info->{text} .= $arrow;
    }

    # Doesn't matter how many prototypes there are, if
    # they haven't happened yet!
    my $declared;
    my $sub_name = $kid_info->{text};
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

    my (@arg_texts, @texts, @body, $type);
    @body = ();
    if ($declared and defined $proto and not $amper) {
	my $args;
	($amper, $args) = $self->check_proto($op, $proto, @exprs);
	if ($amper eq "&") {
	    @body = map($self->deparse($_, 6, $op), @exprs);
	} else {
	    @body = @$args;
	}
    } else {
	@body  = map($self->deparse($_, 6, $op), @exprs);
    }
    @arg_texts = map $_->{text}, @body;
    my $arg_texts = join(', ', @arg_texts);
    if ($prefix or $amper) {
	if ($sub_name eq '&') {
	    # &{&} cannot be written as &&
	    $kid_info->{texts} = ["{", @{$kid_info->{texts}}, "}"];
	    $kid_info->{text} = join('', $kid_info->{texts});
	}
	if ($op->flags & OPf_STACKED) {
	    $type = 'entersub_prefix_or_amper_stacked';
	    @texts = ($prefix, $amper, $sub_name, "(", $arg_texts, ")");
	} else {
	    $type = 'entersub_prefix_or_amper';
	    @texts = ($prefix, $amper, $sub_name);
	}
    } else {
	# It's a syntax error to call CORE::GLOBAL::foo with a prefix,
	# so it must have been translated from a keyword call. Translate
	# it back.
	$sub_name =~ s/^CORE::GLOBAL:://;
	my $dproto = defined($proto) ? $proto : "undefined";
        if (!$declared) {
	    $type = 'entersub_not_declared';
	    @texts = dedup_parens_func($sub_name, @arg_texts);
	} elsif ($dproto =~ /^\s*\z/) {
	    $type = 'entersub_null_proto';
	    @texts = ($sub_name);
	} elsif ($dproto eq "\$" and is_scalar($exprs[0])) {
	    $type = 'entersub_dollar_proto';
	    # is_scalar is an excessively conservative test here:
	    # really, we should be comparing to the precedence of the
	    # top operator of $exprs[0] (ala unop()), but that would
	    # take some major code restructuring to do right.
	    @texts = $self->maybe_parens_func($sub_name, join(', ', @arg_texts), $cx, 16);
	} elsif ($dproto ne '$' and defined($proto) || $simple) { #'
	    $type = 'entersub_proto';
	    @texts = $self->maybe_parens_func($sub_name, join(', ', @arg_texts), $cx, 5);
	} else {
	    $type = 'entersub';
	    @texts = dedup_parens_func($sub_name, @arg_texts);
	}
    }
    return {
	texts => \@texts,
	body => \@body,
	text => join('', @texts),
	type => $type,
	other_ops => $other_ops,
    }
}

sub pp_enterwrite { unop(@_, "write") }

# escape things that cause interpolation in double quotes,
# but not character escapes
sub uninterp {
    my($str) = @_;
    $str =~ s/(^|\G|[^\\])((?:\\\\)*)([\$\@]|\\[uUlLQE])/$1$2\\$3/g;
    return $str;
}

{
my $bal;
BEGIN {
    use re "eval";
    # Matches any string which is balanced with respect to {braces}
    $bal = qr(
      (?:
	[^\\{}]
      | \\\\
      | \\[{}]
      | \{(??{$bal})\}
      )*
    )x;
}

# the same, but treat $|, $), $( and $ at the end of the string differently
sub re_uninterp {
    my($str) = @_;

    $str =~ s/
	  ( ^|\G                  # $1
          | [^\\]
          )

          (                       # $2
            (?:\\\\)*
          )

          (                       # $3
            (\(\?\??\{$bal\}\))   # $4
          | [\$\@]
            (?!\||\)|\(|$)
          | \\[uUlLQE]
          )

	/defined($4) && length($4) ? "$1$2$4" : "$1$2\\$3"/xeg;

    return $str;
}

# This is for regular expressions with the /x modifier
# We have to leave comments unmangled.
sub re_uninterp_extended {
    my($str) = @_;

    $str =~ s/
	  ( ^|\G                  # $1
          | [^\\]
          )

          (                       # $2
            (?:\\\\)*
          )

          (                       # $3
            ( \(\?\??\{$bal\}\)   # $4  (skip over (?{}) and (??{}) blocks)
            | \#[^\n]*            #     (skip over comments)
            )
          | [\$\@]
            (?!\||\)|\(|$|\s)
          | \\[uUlLQE]
          )

	/defined($4) && length($4) ? "$1$2$4" : "$1$2\\$3"/xeg;

    return $str;
}
}

my %unctrl = # portable to EBCDIC
    (
     "\c@" => '\c@',	# unused
     "\cA" => '\cA',
     "\cB" => '\cB',
     "\cC" => '\cC',
     "\cD" => '\cD',
     "\cE" => '\cE',
     "\cF" => '\cF',
     "\cG" => '\cG',
     "\cH" => '\cH',
     "\cI" => '\cI',
     "\cJ" => '\cJ',
     "\cK" => '\cK',
     "\cL" => '\cL',
     "\cM" => '\cM',
     "\cN" => '\cN',
     "\cO" => '\cO',
     "\cP" => '\cP',
     "\cQ" => '\cQ',
     "\cR" => '\cR',
     "\cS" => '\cS',
     "\cT" => '\cT',
     "\cU" => '\cU',
     "\cV" => '\cV',
     "\cW" => '\cW',
     "\cX" => '\cX',
     "\cY" => '\cY',
     "\cZ" => '\cZ',
     "\c[" => '\c[',	# unused
     "\c\\" => '\c\\',	# unused
     "\c]" => '\c]',	# unused
     "\c_" => '\c_',	# unused
    );

# character escapes, but not delimiters that might need to be escaped
sub escape_str { # ASCII, UTF8
    my($str) = @_;
    $str =~ s/(.)/ord($1) > 255 ? sprintf("\\x{%x}", ord($1)) : $1/eg;
    $str =~ s/\a/\\a/g;
#    $str =~ s/\cH/\\b/g; # \b means something different in a regex
    $str =~ s/\t/\\t/g;
    $str =~ s/\n/\\n/g;
    $str =~ s/\e/\\e/g;
    $str =~ s/\f/\\f/g;
    $str =~ s/\r/\\r/g;
    $str =~ s/([\cA-\cZ])/$unctrl{$1}/ge;
    $str =~ s/([[:^print:]])/sprintf("\\%03o", ord($1))/ge;
    return $str;
}

# For regexes with the /x modifier.
# Leave whitespace unmangled.
sub escape_extended_re {
    my($str) = @_;
    $str =~ s/(.)/ord($1) > 255 ? sprintf("\\x{%x}", ord($1)) : $1/eg;
    $str =~ s/([[:^print:]])/
	($1 =~ y! \t\n!!) ? $1 : sprintf("\\%03o", ord($1))/ge;
    $str =~ s/\n/\n\f/g;
    return $str;
}

# Don't do this for regexen
sub unback {
    my($str) = @_;
    $str =~ s/\\/\\\\/g;
    return $str;
}

# Remove backslashes which precede literal control characters,
# to avoid creating ambiguity when we escape the latter.
sub re_unback {
    my($str) = @_;

    # the insane complexity here is due to the behaviour of "\c\"
    $str =~ s/(^|[^\\]|\\c\\)(?<!\\c)\\(\\\\)*(?=[[:^print:]])/$1$2/g;
    return $str;
}

sub balanced_delim {
    my($str) = @_;
    my @str = split //, $str;
    my($ar, $open, $close, $fail, $c, $cnt, $last_bs);
    for $ar (['[',']'], ['(',')'], ['<','>'], ['{','}']) {
	($open, $close) = @$ar;
	$fail = 0; $cnt = 0; $last_bs = 0;
	for $c (@str) {
	    if ($c eq $open) {
		$fail = 1 if $last_bs;
		$cnt++;
	    } elsif ($c eq $close) {
		$fail = 1 if $last_bs;
		$cnt--;
		if ($cnt < 0) {
		    # qq()() isn't ")("
		    $fail = 1;
		    last;
		}
	    }
	    $last_bs = $c eq '\\';
	}
	$fail = 1 if $cnt != 0;
	return ($open, "$open$str$close") if not $fail;
    }
    return ("", $str);
}

sub single_delim {
    my($q, $default, $str) = @_;
    return info_from_list([$default, $str, $default], '', 'single_delim_default', {})
	if $default and index($str, $default) == -1;
    if ($q ne 'qr') {
	(my $succeed, $str) = balanced_delim($str);
	return info_from_list([$q, $str], '', 'single_delim', {}) if $succeed;
    }
    for my $delim ('/', '"', '#') {
	return info_from_list([$q, $delim, $str,
			   $delim], '', 'single_delim_qr', {})
	    if index($str, $delim) == -1;
    }
    if ($default) {
	$str =~ s/$default/\\$default/g;
	return info_from_list([$default, $str, $default], '',
	    'single_delim_qr_esc', {});
    } else {
	$str =~ s[/][\\/]g;
	return info_from_list([$q, '/', $str, '/'], '',
	    'single_delim_qr', {});
    }
}

my $max_prec;
BEGIN { $max_prec = int(0.999 + 8*length(pack("F", 42))*log(2)/log(10)); }

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

sub const {
    my $self = shift;
    my($sv, $cx) = @_;
    if ($self->{'use_dumper'}) {
	return $self->const_dumper($sv, $cx);
    }
    if (class($sv) eq "SPECIAL") {
	# sv_undef, sv_yes, sv_no
	my $text = ('undef', '1', $self->maybe_parens("!1", $cx, 21))[$$sv-1];
	return info_from_text $text, 'const_special', {};
    }
    if (class($sv) eq "NULL") {
	return info_from_text 'undef', 'const_NULL', {};
    }
    # convert a version object into the "v1.2.3" string in its V magic
    if ($sv->FLAGS & SVs_RMG) {
	for (my $mg = $sv->MAGIC; $mg; $mg = $mg->MOREMAGIC) {
	    return $mg->PTR if $mg->TYPE eq 'V';
	}
    }

    if ($sv->FLAGS & SVf_IOK) {
	my $str = $sv->int_value;
	$str = $self->maybe_parens($str, $cx, 21) if $str < 0;
	return info_from_text $str, 'const_INT', {};
    } elsif ($sv->FLAGS & SVf_NOK) {
	my $nv = $sv->NV;
	if ($nv == 0) {
	    if (pack("F", $nv) eq pack("F", 0)) {
		# positive zero
		return info_from_text "0", 'const_plus_zero', {};
	    } else {
		# negative zero
		return info_from_text($self->maybe_parens("-.0", $cx, 21),
				 'const_minus_zero', {});
	    }
	} elsif (1/$nv == 0) {
	    if ($nv > 0) {
		# positive infinity
		return info_from_text($self->maybe_parens("9**9**9", $cx, 22),
				 'const_plus_inf', {});
	    } else {
		# negative infinity
		return info_from_text($self->maybe_parens("-9**9**9", $cx, 21),
				 'const_minus_inf', {});
	    }
	} elsif ($nv != $nv) {
	    # NaN
	    if (pack("F", $nv) eq pack("F", sin(9**9**9))) {
		# the normal kind
		return info_from_text "sin(9**9**9)", 'const_Nan', {};
	    } elsif (pack("F", $nv) eq pack("F", -sin(9**9**9))) {
		# the inverted kind
		return info_from_text($self->maybe_parens("-sin(9**9**9)", $cx, 21),
				 'const_Nan_invert', {});
	    } else {
		# some other kind
		my $hex = unpack("h*", pack("F", $nv));
		return info_from_text(qq'unpack("F", pack("h*", "$hex"))',
				 'const_Na_na_na', {});
	    }
	}
	# first, try the default stringification
	my $str = "$nv";
	if ($str != $nv) {
	    # failing that, try using more precision
	    $str = sprintf("%.${max_prec}g", $nv);
	    # if (pack("F", $str) ne pack("F", $nv)) {
	    if ($str != $nv) {
		# not representable in decimal with whatever sprintf()
		# and atof() Perl is using here.
		my($mant, $exp) = split_float($nv);
		return info_from_text($self->maybe_parens("$mant * 2**$exp", $cx, 19),
				 'const_not_nv', {});
	    }
	}
	$str = $self->maybe_parens($str, $cx, 21) if $nv < 0;
	return info_from_text $str, 'const_nv', {};
    } elsif ($sv->FLAGS & SVf_ROK && $sv->can("RV")) {
	my $ref = $sv->RV;
	if (class($ref) eq "AV") {
	    my $list_info = $self->list_const(2, $ref->ARRAY);
	    return info_from_list(['[', $list_info->{text}, ']'], '', 'const_av',
		{body => $list_info});
	} elsif (class($ref) eq "HV") {
	    my %hash = $ref->ARRAY;
	    my @elts;
	    for my $k (sort keys %hash) {
		push @elts, "$k => " . $self->const($hash{$k}, 6);
	    }
	    return info_from_list(["{", join(", ", @elts), "}"], '', 'const_hv', {});
	} elsif (class($ref) eq "CV") {
	    BEGIN {
		if ($] > 5.0150051) {
		    require overloading;
		    unimport overloading;
		}
	    }
	    if ($] > 5.0150051 && $self->{curcv} &&
		 $self->{curcv}->object_2svref == $ref->object_2svref) {
		return info_from_text($self->keyword("__SUB__"), 'const_sub', {});
	    }
	    my $sub_info = $self->deparse_sub($ref);
	    return info_from_list(["sub ", $sub_info->{text}], '', 'const_sub2',
				  {body => [$sub_info]});
	}
	if ($ref->FLAGS & SVs_SMG) {
	    for (my $mg = $ref->MAGIC; $mg; $mg = $mg->MOREMAGIC) {
		if ($mg->TYPE eq 'r') {
		    my $re = re_uninterp(escape_str(re_unback($mg->precomp)));
		    return single_delim("qr", "", $re);
		}
	    }
	}

	my $const = $self->const($ref, 20);
	if ($self->{in_subst_repl} && $const =~ /^[0-9]/) {
	    $const = "($const)";
	}
	my @texts = ("\\", $const);
	return {
	    text => $self->maybe_parens(join('', @texts), $cx, 20),
	    texts => \@texts,
	    type => 'const_rv' };
    } elsif ($sv->FLAGS & SVf_POK) {
	my $str = $sv->PV;
	if ($str =~ /[[:^print:]]/) {
	    return single_delim("qq", '"', uninterp escape_str unback $str);
	} else {
	    return single_delim("q", "'", unback $str);
	}
    } else {
	return info_from_text "undef", 'const_undef', {};
    }
}

sub const_dumper {
    my $self = shift;
    my($sv, $cx) = @_;
    my $ref = $sv->object_2svref();
    my $dumper = Data::Dumper->new([$$ref], ['$v']);
    $dumper->Purity(1)->Terse(1)->Deparse(1)->Indent(0)->Useqq(1)->Sortkeys(1);
    my $str = $dumper->Dump();
    if ($str =~ /^\$v/) {
	my $texts = ['${my', $str, '\$v}'];
	return {
	    texts => @$texts,
	    text => join(@$texts, ' '),
	};
    } else {
	return { text => $str };
    }
}

sub const_sv {
    my $self = shift;
    my $op = shift;
    my $sv = $op->sv;
    # the constant could be in the pad (under useithreads)
    $sv = $self->padval($op->targ) unless $$sv;
    return $sv;
}

sub pp_const {
    my $self = shift;
    my($op, $cx) = @_;
    if ($op->private & OPpCONST_ARYBASE) {
        return info_from_text '$[', 'const_ary', {};
    }
#    if ($op->private & OPpCONST_BARE) { # trouble with '=>' autoquoting
#	return $self->const_sv($op)->PV;
#    }
    my $sv = $self->const_sv($op);
    return $self->const($sv, $cx);;
}

sub dq {
    my $self = shift;
    my $op = shift;
    my $type = $op->name;
    my $info;
    if ($type eq "const") {
	return info_from_text('$[', 'dq_const_ary', {}) if $op->private & OPpCONST_ARYBASE;
	return info_from_text(uninterp(escape_str(unback($self->const_sv($op)->as_string))),
			 'dq_const', {});
    } elsif ($type eq "concat") {
	my $first = $self->dq($op->first);
	my $last  = $self->dq($op->last);

	# Disambiguate "${foo}bar", "${foo}{bar}", "${foo}[1]", "$foo\::bar"
	($last =~ /^[A-Z\\\^\[\]_?]/ &&
	    $first =~ s/([\$@])\^$/${1}{^}/)  # "${^}W" etc
	    || ($last =~ /^[:'{\[\w_]/ && #'
		$first =~ s/([\$@])([A-Za-z_]\w*)$/${1}{$2}/);

	return info_from_list([$first->{text}, $last->{text}], '', 'dq_concat',
			      {body => [$first, $last]});
    } elsif ($type eq "join") {
	return $self->deparse($op->last, 26); # was join($", @ary)
    } else {
	return $self->deparse($op, 26);
    }
    my $kid = $self->dq($op->first->sibling);
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

sub pp_backtick {
    my $self = shift;
    my($op, $cx) = @_;
    # skip pushmark if it exists (readpipe() vs ``)
    my $child = $op->first->sibling->isa('B::NULL')
	? $op->first : $op->first->sibling;
    if ($self->pure_string($child)) {
	return single_delim("qx", '`', $self->dq($child, 1)->{text});
    }
    unop($self, @_, "readpipe");
}

sub dquote {
    my $self = shift;
    my($op, $cx) = @_;
    my $kid = $op->first->sibling; # skip ex-stringify, pushmark
    return $self->deparse($kid, $cx, $op) if $self->{'unquote'};
    $self->maybe_targmy($kid, $cx,
			sub {single_delim("qq", '"', $self->dq($_[1])->{text})});
}

# OP_STRINGIFY is a listop, but it only ever has one arg
sub pp_stringify { maybe_targmy(@_, \&dquote) }

# tr/// and s/// (and tr[][], tr[]//, tr###, etc)
# note that tr(from)/to/ is OK, but not tr/from/(to)
sub double_delim {
    my($from, $to) = @_;
    my($succeed, $delim);
    if ($from !~ m[/] and $to !~ m[/]) {
	return "/$from/$to/";
    } elsif (($succeed, $from) = balanced_delim($from) and $succeed) {
	if (($succeed, $to) = balanced_delim($to) and $succeed) {
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

sub tr_chr {
    my $x = shift;
    if ($x == ord "-") {
	return "\\-";
    } elsif ($x == ord "\\") {
	return "\\\\";
    } else {
	return chr $x;
    }
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
    return (escape_str($from), escape_str($to));
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
    return info_from_list(['tr', double_delim($from, $to), $flags],
		      '', 'pp_trans', {});
}

sub pp_transr { &pp_trans . 'r' }

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
    my ($op, $extended) = @_;

    my $type = $op->name;
    if ($type eq "const") {
	return '$[' if $op->private & OPpCONST_ARYBASE;
	my $unbacked = re_unback($self->const_sv($op)->as_string);
	return re_uninterp_extended(escape_extended_re($unbacked))
	    if $extended;
	return re_uninterp(escape_str($unbacked));
    } elsif ($type eq "concat") {
	my $first = $self->re_dq($op->first, $extended);
	my $last  = $self->re_dq($op->last,  $extended);
	return re_dq_disambiguate($first, $last);
    } elsif ($type eq "uc") {
	return '\U' . $self->re_dq($op->first->sibling, $extended) . '\E';
    } elsif ($type eq "lc") {
	return '\L' . $self->re_dq($op->first->sibling, $extended) . '\E';
    } elsif ($type eq "ucfirst") {
	return '\u' . $self->re_dq($op->first->sibling, $extended);
    } elsif ($type eq "lcfirst") {
	return '\l' . $self->re_dq($op->first->sibling, $extended);
    } elsif ($type eq "quotemeta") {
	return '\Q' . $self->re_dq($op->first->sibling, $extended) . '\E';
    } elsif ($type eq "fc") {
	return '\F' . $self->re_dq($op->first->sibling, $extended) . '\E';
    } elsif ($type eq "join") {
	return $self->deparse($op->last, 26); # was join($", @ary)
    } else {
	my $ret = $self->deparse($op, 26);
	$ret =~ s/^\$([(|)])\z/\${$1}/; # $( $| $) need braces
	return $ret;
    }
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
    elsif (is_scalar($op) || $type =~ /^[ah]elem$/) {
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

sub regcomp {
    my $self = shift;
    my($op, $cx, $extended) = @_;
    my $kid = $op->first;
    $kid = $kid->first if $kid->name eq "regcmaybe";
    $kid = $kid->first if $kid->name eq "regcreset";
    if ($kid->name eq "null" and !null($kid->first)
	and $kid->first->name eq 'pushmark')
    {
	my $str = '';
	$kid = $kid->first->sibling;
	while (!null($kid)) {
	    my $first = $str;
	    my $last = $self->re_dq($kid, $extended);
	    $str = re_dq_disambiguate($first, $last);
	    $kid = $kid->sibling;
	}
	return $str, 1;
    }

    return ($self->re_dq($kid, $extended), 1) if $self->pure_string($kid);
    return ($self->deparse($kid, $cx), 0);
}

sub pp_regcomp {
    my ($self, $op, $cx) = @_;
    return (($self->regcomp($op, $cx, 0))[0]);
}

sub re_flags {
    my ($self, $op) = @_;
    my $flags = '';
    my $pmflags = $op->pmflags;
    $flags .= "g" if $pmflags & PMf_GLOBAL;
    $flags .= "i" if $pmflags & PMf_FOLD;
    $flags .= "m" if $pmflags & PMf_MULTILINE;
    $flags .= "o" if $pmflags & PMf_KEEP;
    $flags .= "s" if $pmflags & PMf_SINGLELINE;
    $flags .= "x" if $pmflags & PMf_EXTENDED;
    $flags .= "p" if $pmflags & RXf_PMf_KEEPCOPY;
    if (my $charset = $pmflags & RXf_PMf_CHARSET) {
	# Hardcoding this is fragile, but B does not yet export the
	# constants we need.
	$flags .= qw(d l u a aa)[$charset >> 5]
    }
    # The /d flag is indicated by 0; only show it if necessary.
    elsif ($self->{hinthash} and
	     $self->{hinthash}{reflags_charset}
	    || $self->{hinthash}{feature_unicode}
	or $self->{hints} & $feature::hint_mask
	  && ($self->{hints} & $feature::hint_mask)
	       != $feature::hint_mask
	  && do {
		$self->{hints} & $feature::hint_uni8bit;
	     }
  ) {
	$flags .= 'd';
    }
    $flags;
}

# osmic acid -- see osmium tetroxide

my %matchwords;
map($matchwords{join "", sort split //, $_} = $_, 'cig', 'cog', 'cos', 'cogs',
    'cox', 'go', 'is', 'ism', 'iso', 'mig', 'mix', 'osmic', 'ox', 'sic',
    'sig', 'six', 'smog', 'so', 'soc', 'sog', 'xi');

sub matchop {
    my $self = shift;
    my($op, $cx, $name, $delim) = @_;
    my $kid = $op->first;
    my $info = {};
    my ($binop, $var, $re) = ("", "", "");
    if ($op->flags & OPf_STACKED) {
	$binop = 1;
	$var = $self->deparse($kid, 20, $op);
	$info ->{body} = [$var];
	$kid = $kid->sibling;
    }
    my $quote = 1;
    my $pmflags = $op->pmflags;
    my $extended = ($pmflags & PMf_EXTENDED);
    my $rhs_bound_to_defsv;
    if (null $kid) {
	my $unbacked = re_unback($op->precomp);
	if ($extended) {
	    $re = re_uninterp_extended(escape_extended_re($unbacked));
	} else {
	    $re = re_uninterp(escape_str(re_unback($op->precomp)));
	}
    } elsif ($kid->name ne 'regcomp') {
	carp("found ".$kid->name." where regcomp expected");
    } else {
	($re, $quote) = $self->regcomp($kid, 21, $extended);
	my $matchop = $kid->first;
	if ($matchop->name eq 'regcrest') {
	    $matchop = $matchop->first;
	}
	if ($matchop->name =~ /^(?:match|transr?|subst)\z/
	   && $matchop->flags & OPf_SPECIAL) {
	    $rhs_bound_to_defsv = 1;
	}
    }
    my $flags = "";
    $flags .= "c" if $pmflags & PMf_CONTINUE;
    $flags .= $self->re_flags($op);
    $flags = join '', sort split //, $flags;
    $flags = $matchwords{$flags} if $matchwords{$flags};
    if ($pmflags & PMf_ONCE) { # only one kind of delimiter works here
	$re =~ s/\?/\\?/g;
	$re = "?$re?";
    } elsif ($quote) {
	$re = single_delim($name, $delim, $re)->{text};
    }
    my @re = ($re);
    push @re, $flags if $quote;
    my @texts;
    if ($binop) {
	if ($rhs_bound_to_defsv) {
	    @texts = ($var, ' =~ ', "(", '$_', ' =~ ', @re, ')');
	} else {
	    @texts = ($var, ' =~ ', $re);
	}
	$info->{text} = $self->maybe_parens(join('', @texts), $cx, 20);
	$info->{type} = 'matchop_binop';
    } else {
	@texts = ($re);
	$info->{text} = $re;
	$info->{type} = 'matchop_unop';
    }
    $info->{texts} = \@texts;
    $info->{text} = join('', @texts);
    return $info;
}

sub pp_match { matchop(@_, "m", "/") }
sub pp_pushre { matchop(@_, "m", "/") }
sub pp_qr { matchop(@_, "qr", "") }

sub pp_runcv { unop(@_, "__SUB__"); }

sub pp_split {
    my $self = shift;
    my($op, $cx) = @_;
    my($kid, @exprs, $ary, $expr);
    $kid = $op->first;

    # For our kid (an OP_PUSHRE), pmreplroot is never actually the
    # root of a replacement; it's either empty, or abused to point to
    # the GV for an array we split into (an optimization to save
    # assignment overhead). Depending on whether we're using ithreads,
    # this OP* holds either a GV* or a PADOFFSET. Luckily, B.xs
    # figures out for us which it is.
    my $replroot = $kid->pmreplroot;
    my $gv = 0;
    if (ref($replroot) eq "B::GV") {
	$gv = $replroot;
    } elsif (!ref($replroot) and $replroot > 0) {
	$gv = $self->padval($replroot);
    }
    $ary = $self->stash_variable('@', $self->gv_name($gv), $cx) if $gv;

    for (; !null($kid); $kid = $kid->sibling) {
	push @exprs, $self->deparse($kid, 6);
    }

    # handle special case of split(), and split(' ') that compiles to /\s+/
    # Under 5.10, the reflags may be undef if the split regexp isn't a constant
    # Under 5.17.5-5.17.9, the special flag is on split itself.
    $kid = $op->first;
    if ( $op->flags & OPf_SPECIAL
         or (
            $kid->flags & OPf_SPECIAL
            and ( $] < 5.009 ? $kid->pmflags & PMf_SKIPWHITE()
                             : ($kid->reflags || 0) & RXf_SKIPWHITE()
            )
         )
    ) {
	$exprs[0] = "' '";
    }

    $expr = "split(" . join(", ", @exprs) . ")";
    if ($ary) {
	return $self->maybe_parens("$ary = $expr", $cx, 7);
    } else {
	return $expr;
    }
}

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
    my ($repl, $repl_info);
    if ($op->flags & OPf_STACKED) {
	$binop = 1;
	$var = $self->deparse($kid, 20);
	$kid = $kid->sibling;
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
    my $extended = ($pmflags & PMf_EXTENDED);
    if (null $kid) {
	my $unbacked = re_unback($op->precomp);
	if ($extended) {
	    $re = re_uninterp_extended(escape_extended_re($unbacked));
	}
	else {
	    $re = re_uninterp(escape_str($unbacked));
	}
    } else {
	($re) = $self->regcomp($kid, 1, $extended);
    }
    $flags .= "r" if $pmflags & PMf_NONDESTRUCT;
    $flags .= "e" if $pmflags & PMf_EVAL;
    $flags .= $self->re_flags($op);
    $flags = join '', sort split //, $flags;
    $flags = $substwords{$flags} if $substwords{$flags};
    my $info;
    my $repl_text = $repl_info->{text};
    my $opts = {body => [$repl_info]};
    $opts->{other_ops} = \@other_ops if @other_ops;
    if ($binop) {
	my @texts = ("$var", " ", "=~", " ", "s", double_delim($re, $repl_text), $flags);
	$opts->{maybe_parens} = [$self, $cx, 20];
	return info_from_list(\@texts, '', 'subst_binop', $opts);
    } else {
	return info_from_list(['s', double_delim($re, $repl_text)], '', 'subst',
			      $opts);
    }
    Carp::confess("unhandled condition in pp_subst");
}

sub is_lexical_subs {
    my (@ops) = shift;
    for my $op (@ops) {
        return 0 if $op->name !~ /\A(?:introcv|clonecv)\z/;
    }
    return 1;
}

sub pp_introcv {
    my $self = shift;
    my($op, $cx) = @_;
    # For now, deparsing doesn't worry about the distinction between introcv
    # and clonecv, so pretend this op doesn't exist:
    return '';
}

sub pp_clonecv {
    my $self = shift;
    my($op, $cx) = @_;
    my $sv = $self->padname_sv($op->targ);
    my $name = substr $sv->PVX, 1; # skip &/$/@/%, like $self->padany
    return "my sub $name";
}

sub pp_padcv {
    my $self = shift;
    my($op, $cx) = @_;
    return $self->padany($op);
}

1;

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
	    my $foo = "Ab\x{100}\200\x{200}\237Cd\000Ef\x{1000}\cA\x{2000}\cZ";
	}
    };

    my $deparse = __PACKAGE__->new("-p", "-l", "-c", "-sC");
    my $info = $deparse->coderef2list(\&baz);
    import Data::Printer colored => 0;
    Data::Printer::p($info);
    print "\n", '=' x 30, "\n";
    # print $deparse->coderef2text(\&bar);
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
    # use B::Deparse;
    # my $deparse_old = B::Deparse->new("-p", "-l", "-sC");
    # print $deparse_old->coderef2text(\&fib);
}
