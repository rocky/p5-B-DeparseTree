#!/bin/bash
PERL_VERSIONS="5.18.4 5.20.3 5.22.2 5.24.4 5.26.3"
cd ..
for v in $PERL_VERSIONS ; do
    ./Build clean
    perlbrew use $v
    perl Build.PL
    ./Build
    make check
done
