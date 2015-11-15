# The underlying node structure of the abstract code tree built
# that is built.
# Copyright (c) 2015 Rocky Bernstein
use strict; use warnings;
package B::DeparseTree::Node;
use Carp;

our($VERSION, @EXPORT, @ISA);
$VERSION = '1.0.0';
@ISA = qw(Exporter);
@EXPORT = qw(new($$$$ from_str($$$) from_list($$$) parens_test($$$)));

=head2 Node structure

Fields in a node structure:

=over

*item B<type>

The string name for the node. It can be used to determine the overall
structure. For example a 'binop' node will have a I<body> with a node
left-hand side, the string operation name and a I<body> right-hand
side. Right now the type names are a little funky, but over time I
hope these will less so.

* item B<sep>

A string indicating how to separate the the strings derived from
the body. To indicate statement separation, the separator is ';' and the
B<indent_type> is '{'. The indent program can also use the type
to help out with statement boundaries.

* item B<body>

A list reference to either a string, a Node reference, or a Hash reference
containing the keys I<sep> and a I<body>.

* item B<text>

I<Note: this is temporary.> Text representation of the node until we
get everything coverted. In the future, you'll use one of the node-to-string
conversion routines.

* item B<maybe_parens>

If this node is embedded in the parent above, whether we need to add parenthesis.
The kesy is a hash ref hash reference

=over

=item B<context>

A number passed from the parent indicating its precidence context

=item B<precidence>

A number as determined by the operator at this level.

=item B<parens>

'true' if we should to add parenthesis based on I<context> and
I<precidence> values; '' if not.

=back

* item B<indent_type>

If this is exists, then the value is '{' or '('.
The first letter indicates the inclosing delimiters, '{' and '}' for '{';
'(' and ')' for '('. The separator here should either be ',' or ';'. A ';' separator
with a '{'  indicates
separation with an additional newline and an added indentation level for the first
in the list. Otherwise fields will be filled to the to the maximum line width
and wrapped.


=back
=cut


sub parens_test($$$)
{
    my ($obj, $cx, $prec) = @_;
    return ($prec < $cx or
	    $obj->{'parens'} or
	    # unary ops nest just fine
	    $prec == $cx and $cx != 4 and $cx != 16 and $cx != 21)
}

sub new($$$$$)
{
    my ($class, $item, $sep, $type, $opts) = @_;
    my $self = bless {
	body => $item,
	type => $type,
	sep => $sep,
    }, $class;

    $self->{text} = $self->combine($sep, $item);

    foreach my $optname (qw(other_ops parent_ops)) {
	$self->{$optname} = $opts->{$optname} if $opts->{$optname};
    }
    if ($opts->{maybe_parens}) {
	my ($obj, $context, $precidence) = @{$opts->{maybe_parens}};
	my $parens = parens_test($obj, $context, $precidence);
	$self->{maybe_parens} = {
	    context => $context,
	    precidence => $precidence,
	    force => $obj->{'parens'},
	    parens => $parens ? 'true' : ''
	};
    }
    return $self;
}

# Simplified class constructors
sub from_str($$$)
{
    my ($str, $type, $opts) = @_;
    __PACKAGE__->new({body=>[$str]}, '', $type, $opts);
}

sub from_list($$$$)
{
    my ($list, $sep, $type, $opts) = @_;
    __PACKAGE__->new({body=>$list}, $sep, $type, $opts);
}

sub combine($$$)
{
    my ($self, $sep, $items) = @_;
    # FIXME: loop over $item, testing type.
    return $items unless ref $items;
    Carp::confess("should be a reference to a hash: is $items") unless
	ref $items eq 'HASH';
    my $result = '';
    foreach my $item (@{$items->{body}}) {
	my $add;
	if (ref $item) {
	    $add = $self->combine($item->{sep}, $item->{body});
	} else {
	    $add = $item;
	}
	if ($result) {
	    $result .= ($sep . $add);
	} else {
	    $result = $add;
	}
    }
    return $result;
}

# FIXME: replace with routines to build text on from the tree
#
sub text($)
{
    return shift()->{text};
}


# Possibly add () around $text depending on precidence $prec and
# context $cx. We return a string.
sub maybe_parens($$$$)
{
    my($self, $info, $cx, $prec) = @_;
    if (parens_test($info, $cx, $prec)) {
	$info->{text} = $self->combine('', "(", $info->{text}, ")");
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

unless(caller) {
    *fs = \&B::DeparseTree::Node::from_str;
    *fl = \&B::DeparseTree::Node::from_list;
    my @list = ();
    push @list, fs("X", 'string', {}),
	fl(['A', 'B'], ':', 'simple-list', {});
    push @list, fl(\@list, '||', 'compound-list', {});
    foreach my $item (@list) {
	print $item->text, "\n";
    }
}
1;
