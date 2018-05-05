package B::DeparseTree::Fragment;

use vars qw(@ISA @EXPORT);
@ISA = ('Exporter');
@EXPORT = qw(deparse_offset get_addr_info get_parent_addr);

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

sub get_addr_info($$)
{
    my ($deparse, $addr) = @_;
    return unless $addr;
    my $op_info = $deparse->{optree}{$addr};
    if ($op_info) {
	# use Data::Printer; Data::Printer::p $op_info;
	# my $text = $deparse->indent_info($op_info);
	return $op_info;
    }
    return undef;
}

sub get_parent_addr_info($)
{
    my ($op_info) = @_;

    return undef unless $op_info && $op_info->{parent};
    my $deparse = $op_info->{deparse};
    return undef unless $deparse;
    my $parent_addr = $op_info->{parent};
    return $deparse->{optree}{$parent_addr};
}


sub extract_node_info
{
    my ($info) = @_;
    my $parent = $info->{parent} ? $info->{parent} : undef;
    my $deparsed = $info->{deparse};
    # FIXME: start here.
}

unless (caller) {
    sub bug() {
	print sort(foo('bar'));
    }

    my $deparse = B::DeparseTree->new();
    $deparse->coderef2info(\&bug);
    my @addrs = sort keys %{$deparse->{optree}}, "\n";
    use Data::Printer;
    for (my $i=0; $i<scalar(@addrs); $i++) {
	my $info = get_addr_info($deparse, $addrs[$i]);
	if ($info) {
	    printf "0x%0x\n", $addrs[$i];
	    p $info ;
	}
	if ($info->{parent}) {
	    my $parent = get_parent_addr_info($info);
	    if ($info) {
		p $parent ;
	    }
	}
	print '-' x 50, "\n";
    }
}

1;
