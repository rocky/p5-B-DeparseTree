package B::DeparseTree::Fragment;

use strict; use warnings;
use Data::Printer;
use vars qw(@ISA @EXPORT);
@ISA = ('Exporter');
@EXPORT = qw(deparse_offset
             dump
             extract_node_info
             get_addr_info
             get_parent_addr_info get_parent_op
             get_prev_addr_info   get_prev_op);

sub deparse_offset
{
    my ($funcname, $address) = @_;

    my $deparse = B::DeparseTree->new();
    if ($funcname eq "DB::DB") {
	$deparse->main2info;
    } else {
	$deparse->coderef2info(\&$funcname);
    }
    get_addr_info($deparse, $address);
}

sub get_addr($$)
{
    my ($deparse, $addr) = @_;
    return undef unless $addr;
    return $deparse->{optree}{$addr};
}

sub get_addr_info($$)
{
    my ($deparse, $addr) = @_;
    my $op_info = get_addr($deparse, $addr);
    return $op_info;
}

sub get_parent_op($)
{
    my ($op_info) = @_;
    return undef unless $op_info;
    my $deparse = $op_info->{deparse};

    # FIXME:
    return $deparse->{ops}{$op_info->{addr}}{parent};
}

sub get_parent_addr_info($)
{
    my ($op_info) = @_;
    my $deparse = $op_info->{deparse};
    # FIXME
    # my $parent_op = get_parent_op($op_info);
    my $parent_addr = $op_info->{parent};
    return undef unless $parent_addr;
    return $deparse->{optree}{$parent_addr};
}

sub get_prev_op($)
{
    my ($op_info) = @_;
    return undef unless $op_info;
    my $deparse = $op_info->{deparse};
    my $ref = $deparse->{ops}{$op_info->{addr}};
    return $ref->{prev_op} ? $ref : undef;
}

sub get_prev_addr_info($)
{
    my ($op_info) = @_;
    my $deparse = $op_info->{deparse};
    my $prev_op = get_prev_op($op_info);
    return undef unless $prev_op;
    return $deparse->{optree}{$$prev_op};
}

sub trim_line_pair($$$$) {
    my ($parent_text, $child_text, $parent_underline, $start_pos) = @_;
    # If the parent text is longer than a line, use just the line.
    # The underline indicator adds an elipsis to show it is elided.
    my @parent_lines = split(/\n/, substr($parent_text, $start_pos));
    my $stripped_parent = $parent_lines[0];
    if (length($parent_underline) > length($stripped_parent)) {
	$parent_underline = substr($parent_underline, 0, length($stripped_parent)) . '...';
    }
    return [$stripped_parent, $parent_underline];
}

sub extract_node_info($)
{
    my ($info) = @_;
    my $child_text = $info->{text};
    my $parent = $info->{parent} ? $info->{parent} : undef;
    return [$child_text] unless $parent;
    my $child_addr = $info->{addr};
    my $deparsed = $info->{deparse};
    my $parent_info = $deparsed->{optree}{$parent};
    return [$child_text] unless $parent_info;
    my $separator = $parent_info->{sep};
    my @texts = @{$parent_info->{texts}};
    my $parent_line = '';
    my $text_len = $#texts;
	my $result = '';

    if (!exists $parent_info->{fmt}
	and scalar(@texts) == 1
	and eval{$texts[0]->isa("B::DeparseTree::Node")}) {
	$parent_info = $texts[0];
    }
    if (exists $parent_info->{fmt}) {
	my $fmt = $parent_info->{fmt};
	my $indexes = $parent_info->{indexes};
	my $args = $parent_info->{texts};
	my ($str, $found_pos) = $deparsed->template_engine($fmt, $indexes, $args,
							   $child_addr);
	if (defined($found_pos)) {
	    my $parent_underline = ' ' x $found_pos->[0];
	    $parent_underline .= '-' x $found_pos->[1];
	    return trim_line_pair($str, $child_text, $parent_underline, 0);
	}
	$result = $str;
    } else {
	for (my $i=0; $i <= $text_len; $i++) {
	    my $text = $texts[$i];
	    $result .= $separator if $result;

	    if (ref($text)) {
		if (ref($text) eq 'ARRAY' and (scalar(@$text) == 2)) {
		    if ($text->[1] == $child_addr) {
			$child_text = $text->[0];
			my $parent_underline = ' ' x length($result);
			$result .= $text->[0];
			$parent_underline .= '-' x length($text->[0]);
			if ($i < $text_len) {
			    $result .= $separator;
			    my @remain_texts = @texts[$i+1..$#texts];
			    my $tail = $deparsed->combine2str($separator, \@remain_texts);
			    $result .=  $tail;
			}
			return trim_line_pair($result, $child_text, $parent_underline, 0);
		    } else {
			$result .= $text->[0];
		    }
		} elsif ($text->{addr} == $child_addr) {
		    # WARNING: this branch is going away when we have
		    # everything templatized (when not a simple string).
		    my $parent_underline = ' ' x length($result);
		    $result .= $text->{text};
		    $parent_underline .= '-' x length($text->{text});
		    if ($i < $text_len) {
			$result .= $separator;
			my @remain_texts = @texts[$i+1..$#texts];
			my $tail = $deparsed->combine2str($separator, \@remain_texts);
			$result .=  $tail;
		    }
		    return trim_line_pair($result, $child_text, $parent_underline, 0);
		} else {
		    $result .= $text->{text};
		}
	    } else {
		$result .= $text;
	    }
	}
    }
    # Can't find by node address info, so just try to find the string
    # inside of the parent.
    my $parent_text = $parent_info->{text};
    my $start_index = index($parent_text, $child_text);
    if ($start_index >= 0) {
	if (index($parent_text, $child_text, $start_index+1) < 0) {
	    # It is in there *uniquely*!
	    # Remove any \n's before the text.
	    my $last_pos = 0;
	    my $start_search = index($parent_text, $child_text);
	    while ($start_search >= 0) {
		$last_pos = $start_search;
		$start_search = index($parent_text, $child_text, $last_pos+1);
	    }
	    my $start_pos = ($start_index - $last_pos);
	    my $parent_underline = ' ' x $start_pos ;

	    $parent_underline .= '~' x length($child_text);
	    return trim_line_pair($parent_text, $child_text, $parent_underline, $start_index);
	}
    }
}

# Dump out the entire list of texts
sub dump($) {
    my ($deparse_tree) = @_;
    my @addrs = sort keys %{$deparse_tree->{optree}};
    for (my $i=0; $i < $#addrs; $i++) {
	print $i, '-' x 50, "\n";
	my $info = get_addr_info($deparse_tree, $addrs[$i]);
	if ($info) {
	    printf "0x%0x\n", $addrs[$i];
	    p $info ;
	}
	if ($info->{parent}) {
	    my $parent = get_parent_addr_info($info);
	    if ($parent) {
		p $parent ;
		my $texts = extract_node_info($info);
		if ($texts) {
		    print join("\n", @$texts), "\n";
		}
	    }
	}
	print $i, '-' x 50, "\n";
    }
}

unless (caller) {
    sub bug() {
	return 5
	# no strict;
	# for ( $i=0; $i;) {};
	# my ($a, $b, $c);
	# CORE::exec($foo $bar);
	# exec $foo $bar;
	# exec $foo $bar;
    }

    my $child_text = '$foo $bar';
    my $result = 'exec $foo $bar';
    my $parent_underline = "     ---------";
    my $start_pos = 0;
    my $lines = trim_line_pair($result, $child_text, $parent_underline,
			       $start_pos);
    print join("\n", @$lines), "\n";

    my $deparse = B::DeparseTree->new();
    # use B;
    # $deparse->pessimise(B::main_root, B::main_start);
    # my @addrs = sort keys %{$deparse->{ops}}, "\n";
    # use Data::Printer;
    # p @addrs;

    # my @info_addrs = sort keys %{$deparse->{optree}}, "\n";
    # print '-' x 40, "\n";
    # p @info_addrs;

    # $deparse->init();
    # my $svref = B::svref_2object(\&bug);
    # my $x =  $deparse->deparse_sub($svref, $addrs[9]);
    # p $x;

    # # my @info_addrs = sort keys %{$deparse->{optree}}, "\n";
    # # print '-' x 40, "\n";
    # # p @info_addrs;

    # #my $info = get_addr_info($deparse, $addrs[10]);
    # # p $info;
    # exit 0;

    $deparse->coderef2info(\&bug);
    # $deparse->coderef2info(\&get_addr_info);
    my @addrs = sort keys %{$deparse->{optree}}, "\n";
    B::DeparseTree::Fragment::dump($deparse);
}

1;
