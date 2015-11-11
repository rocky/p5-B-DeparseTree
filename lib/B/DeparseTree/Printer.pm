# Copyright (c) 2015 Rocky Bernstein
use strict; use warnings;
use rlib '../..';
use B::DeparseTree::Common;

package B::DeparseTree::Printer;

use constant sep_string => ('=' x 40) . "\n";

# Elide string with ... if it is too long, and
# show control characters in string.
sub short_str($) {
    my $str = shift;
    if (length($str) > 20) {
	$str = substr($str, 0, 7) . '...' . substr($str, -7);
    }
    $str =~ s/\cK/\\cK/g;
    $str =~ s/\f/\\f/g;
    $str =~ s/\n/\\n/g;
    $str =~ s/\t/\\t/g;
    return $str
}

sub format_info_short($$)
{
    my ($info, $show_body) = @_;

    my %i = %{$info};
    my $text;
    my $op = $i{op};
    if ($op) {
	$text = sprintf(
	    "0x%x %s/%s: \"%s\"",
	    $$op,
	    $i{type},
	    $op->name,
	    short_str($i{text}));
    } else {
	$text = sprintf(
	    "?  %s: \"%s\"%s",
	    $i{type},
	    short_str($i{text}));
    }
    if (exists $i{maybe_parens}) {
	my %mp = %{$i{maybe_parens}};
	if (B::DeparseTree::Common::parens_test($mp{cx}, $mp{prec})) {
	    $text .= ' - parens';
	}
    }
    if (exists $i{body} and $show_body) {
	$text .= ("\n\t" .
		  join(",\n\t",
		       map(sprintf("0x%x %s '%s'", ${$_->{op}},
				   $_->{type}, short_str($_->{text})),
			   @{$i{body}})));
    }
    # FIXME: other ops
    return $text;
}

sub format_info($)
{
    my $info = shift;
    my %i = %{$info};
    my $fmt = <<EOF;
type    :%s
op      :%s
cop line: %s
parent  : %s
text:
%s
---------
sep: '%s'
EOF
    my $text = sprintf($fmt,
		       $i{type},
		       $i{op} ? $i{op}->name   : '???',
		       $i{cop} ? $i{cop}->line : 'none',
		       exists $i{parent} ? sprintf("0x%x", $i{parent}) : 'no parent',
		       $i{text},
		       $i{sep});
    my @texts = @{${i}{texts}};
    for (my $j=0; $j < scalar @texts; $j++) {
	my $line = short_str($texts[$j]);
	$text .= "text[$j]: \"$line\"\n";

    }
    $text .= sep_string;
    if ($i{body}) {
	my @body = @{$i{body}};
	for (my $j=0; $j < scalar @body; $j++) {
	    $text .= sprintf("body[$j]: %s\n", format_info_short($body[$j], 1));
	}
    }
    if ($i{maybe_parens}) {
	$text .= sep_string;
	my @fields = ('parent context', 'op precidence');
	my @maybe_parens = @{$i{maybe_parens}};
	for (my $j=0; $j < scalar @fields; $j++) {
	    $text .= sprintf "%s: %d\n", $fields[$j], $maybe_parens[$j];
	}
	$text .= sprintf "need parens: %s\n", parens_test(@maybe_parens) ? 'yes' : 'no';
    }
    # FIXME: other ops
    return $text;
}

sub format_info_walk($$);
sub format_info_walk($$)
{
    my ($info, $indent_level) = @_;
    my $text = '';
    $text = format_info_short($info, 0);
    $indent_level += 2;
    return $text unless exists $info->{body};
    my @body = @{$info->{body}};
    for (my $i=0; $i < scalar @body; $i++) {
	my $info = $body[$i];
	my $lead = "\n" . (' ' x $indent_level) . "[$i] ";
	$text .= ($lead . format_info_walk($info, $indent_level));
    }
    return $text;
}

unless(caller) {
    require B::DeparseTree;
    eval {
	sub fib {
	    my $x = shift;
	    return 1 if $x <= 1;
	    return(fib($x-1) + fib($x-2))
	}
    };
    my $deparse = B::DeparseTree->new("-p", "-l", "-c", "-sC");
    my $info = $deparse->coderef2list(\&fib);
    print format_info($info), "\n";
    print format_info_short($info, 1), "\n";
    print '*' x 30 . "\n";
    print format_info_walk($info, 0), "\n";
}

1;
