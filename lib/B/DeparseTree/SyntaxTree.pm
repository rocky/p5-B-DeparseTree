# B::DeparseTree tree-building routines.
# Copyright (c) 2018 Rocky Bernstein
# All rights reserved.
# This module is free software; you can redistribute and/or modify
# it under the same terms as Perl itself.

# This is based on the module B::Deparse by Stephen McCamant.
# It has been extended save tree structure, and is addressible
# by opcode address.

# Note the package name. It is *not* B::DeparseTree::Tree.
package B::DeparseTree::SyntaxTree;

use B::DeparseTree::Node;

our($VERSION, @EXPORT, @ISA);
$VERSION = '3.1.1';
@ISA = qw(Exporter B::DeparseTree);
@EXPORT = qw(
    combine
    combine2str
    indent_less
    indent_more
    indent_value
    info2str
    info_from_list
    info_from_template
    info_from_string
    info_from_text
    );

sub combine($$$)
{
    my ($self, $sep, $items) = @_;
    # FIXME: loop over $item, testing type.
    Carp::confess("should be a reference to a array: is $items") unless
	ref $items eq 'ARRAY';
    my @result = ();
    foreach my $item (@$items) {
	my $add;
	if (ref $item) {
	    if (ref $item eq 'ARRAY' and scalar(@$item) == 2) {
		$add = [$item->[0], $item->[1]];
	    } elsif (eval{$item->isa("B::DeparseTree::Node")}) {
		$add = [$item->{text}, $item->{addr}];
		# First item is text and second item is op address.
	    } else {
		Carp::confess("don't know what to do with $item");
	    }
	} else {
	    $add = $item;
	}
	push @result, $sep if @result && $sep;
	push @result, $add;
    }
    return @result;
}

sub combine2str($$$)
{
    my ($self, $sep, $items) = @_;
    my $result = '';
    foreach my $item (@$items) {
	$result .= $sep if $result;
	if (ref $item) {
	    if (ref $item eq 'ARRAY' and scalar(@$item) == 2) {
		# First item is text and second item is op address.
		$result .= $self->info2str($item->[0]);
	    } elsif (eval{$item->isa("B::DeparseTree::Node")}) {
		if (exists $item->{fmt}) {
		    $result .= $self->template2str($item);
		} else {
		    $result .= $self->info2str($item);
		}
	    } else {
		Carp::confess("Invalid ref item ref($item)");
	    }
	} else {
	    # FIXME: add this and remove errors
	    if (index($item, '@B::DeparseTree::Node') > 0) {
	    	Carp::confess("\@B::DeparseTree::Node as an item is probably wrong");
	    }
	    $result .= $item;
	}
    }
    return $result;
}

sub indent_less($$) {
    my ($self, $check_level) = @_;
    $check_level = 0 if !defined $check_level;

    $self->{level} -= $self->{'indent_size'};
    my $level = $self->{level};
    if ($check_level < 0) {
	Carp::confess("mismatched indent/dedent") if $check_level;
	$level = 0;
	$self->{level} = 0;
    }
    return $self->indent_value();
}

sub indent_more($) {
    my ($self) = @_;
    $self->{level} += $self->{'indent_size'};
    return $self->indent_value();
}

sub indent_value($) {
    my ($self) = @_;
    my $level = $self->{level};
    if ($self->{'use_tabs'}) {
	return "\t" x ($level / 8) . " " x ($level % 8);
    } else {
	return " " x $level;
    }
}

sub info2str($$)
{
    my ($self, $item) = @_;
    my $result = '';
    if (ref $item) {
	if (ref $item eq 'ARRAY' and scalar(@$item) == 2) {
	    # First item is text and second item is op address.
	    $result = $item->[0];
	} elsif (eval{$item->isa("B::DeparseTree::Node")}) {
	    if (exists $item->{fmt}) {
		$result .= $self->template2str($item);
		if ($item->{maybe_parens}) {
		    my $mp = $item->{maybe_parens};
		    if ($mp->{force} || $mp->{parens}) {
			$result = "($result)";
		    }
		}
	    } elsif (!exists $item->{texts} && exists $item->{text}) {
		# Is a constant string
		$result .= $item->{text};
	    } else {
		$result = $self->combine2str($item->{sep},
					     $item->{texts});
	    }

	} else {
	    Carp::confess("Invalid ref item ref($item)");
	}
    } else {
	# FIXME: add this and remove errors
	if (index($item, '@B::DeparseTree::Node') > 0) {
		Carp::confess("\@B::DeparseTree::Node as an item is probably wrong");
	}
	$result = $item;
    }
    return $result;
}

# Create an info structure from a list of strings
# FIXME: $deparse (or rather $self) should be first
sub info_from_list($$$$$$)
{
    my ($op, $self, $texts, $sep, $type, $opts) = @_;

    # Set undef in "texts" argument position because we are going to create
    # our own text from the $texts.
    my $info = B::DeparseTree::Node->new($op, $self, $texts, undef,
					 $type, $opts);
    $info->{sep} = $sep;
    my $text = '';
    foreach my $item (@$texts) {
	$text .= $sep if $text and $sep;
	if(ref($item) eq 'ARRAY'){
	    $text .= $item->[0];
	} elsif (eval{$item->isa("B::DeparseTree::Node")}) {
	    $text .= $item->{text};
	} else {
	    $text .= $item;
	}
    }

    $info->{text} = $text;
    if ($opts->{maybe_parens}) {
	my ($obj, $context, $precedence) = @{$opts->{maybe_parens}};
	my $parens = B::DeparseTree::Node::parens_test($obj, $context, $precedence);
	$self->{maybe_parens} = {
	    context => $context,
	    precedence => $precedence,
	    force => $obj->{'parens'},
	    parens => $parens ? 'true' : ''
	};
	$info->{text} = "($info->{text})" if exists $info->{text} and $parens;
    }

    return $info
}

# Create an info structure a template pattern
sub info_from_template($$$$$) {
    my ($self, $type, $op, $fmt, $indexes, $args, $opts) = @_;
    $opts = {} unless defined($opts);
    my $text = $self->template_engine($fmt, $indexes, $args);
    my $info = B::DeparseTree::Node->new($op, $self, $args, undef, $type, $opts);
    $info->{'fmt'}  = $fmt;
    $info->{'indexes'} = $indexes if $indexes;
    $info->{'text'} = $self->template_engine($fmt, $indexes, $args);

    if (! defined $op) {
	$info->{addr} = ++$self->{'last_fake_addr'};
	$self->{optree}{$info->{addr}} = $info;
    }

    if ($opts->{'relink_children'}) {
	# FIXME we should specify which children to relink
	for (my $i=0; $i < scalar @$args; $i++) {
	    if ($args->[$i]->isa("B::DeparseTree::Node")) {
		$args->[$i]{parent} = $info->{addr};
	    }
	}
    }

    # Link the parent of Deparse::Tree::Nodes to this node.
    if ($opts->{'synthesized_nodes'}) {
	foreach my $node (@{$opts->{'synthesized_nodes'}}) {
	    $node->{parent} = $info->{addr};
	}
    }

    # Need to handle maybe_parens since B::DeparseNode couldn't do that
    # as it was passed a ref ARRAY rather than a string.
    if ($opts->{maybe_parens}) {
	my ($obj, $context, $precedence) = @{$opts->{maybe_parens}};
	my $parens = B::DeparseTree::Node::parens_test($obj,
						       $context, $precedence);
	$info->{maybe_parens} = {
	    context => $context,
	    precedence => $precedence,
	    force => $obj->{'parens'},
	    parens => $parens ? 'true' : ''
	};
	$info->{text} = "($info->{text})" if exists $info->{text} and $parens;
    }

    return $info;
}

# Create an info structure from a single string
sub info_from_string($$$$$)
{
    my ($self, $type, $op, $str, $opts) = @_;
    $opts ||= {};
    return B::DeparseTree::Node->new($op, $self, $str, undef,
				     $type, $opts);
}

# OBSOLETE: Create an info structure from a single string
# FIXME: remove this
sub info_from_text($$$$$)
{
    my ($op, $self, $text, $type, $opts) = @_;
    # Use this to smoke outt calls
    # use Enbugger 'trepan'; Enbugger->stop;
    return $self->info_from_string($type, $op, $text, $opts)
}
1;
