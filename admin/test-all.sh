#!/bin/bash

bs=${BASH_SOURCE[0]}
if [[ $0 == $bs ]] ; then
    echo "This script should be *sourced* rather than run directly through bash"
    exit 1
fi

PERL_VERSIONS="5.26.2 5.24.4 5.22.4 5.20.3 5.18.4"
for v in $PERL_VERSIONS ; do
    perlbrew use perl-$v && perl ./Build.PL && make && make check && make install
done
