#  Copyright (C) 2011, 2013, 2018 Rocky Bernstein <rocky@cpan.org>
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#     PREREQ_PM => { Test::More=>q[0], version=>q[0], ExtUtils::PkgConfig=>q[1.03] }

# Note: remake (GNU make with debugging) has a --tasks option which
# will show important targets. In remake, to give a target a
# user-friendly description, one starts a comment line with #:

GIT2CL ?= git2cl

#: Build everything
all:
	perl ./Build --makefile_env_macros 1

#: Build program, e.g. copy to blib
build:
	perl Build --makefile_env_macros 1 build

#: Remove automatically generated files
clean:
	perl Build --makefile_env_macros 1 clean

code:
	perl Build --makefile_env_macros 1 code

config_data:
	perl Build --makefile_env_macros 1 config_data

diff:
	perl ./Build --makefile_env_macros 1 diff

#: Create distribution tarball
dist:
	perl ./Build --makefile_env_macros 1 dist

distcheck:
	perl ./Build --makefile_env_macros 1 distcheck

distclean:
	perl ./Build --makefile_env_macros 1 distclean

distdir:
	perl ./Build --makefile_env_macros 1 distdir

distmeta:
	perl ./Build --makefile_env_macros 1 distmeta

distsign:
	perl Build --makefile_env_macros 1 distsign

disttest:
	perl Build --makefile_env_macros 1 disttest

#: Create documentation (in blib/libdoc) via perlpod
docs:
	perl Build --makefile_env_macros 1 docs

fakeinstall:
	perl Build --makefile_env_macros 1 fakeinstall

#: Show help
help:
	perl Build --makefile_env_macros 1 help

html:
	perl Build --makefile_env_macros 1 html

#: Install this puppy
install:
	perl Build --makefile_env_macros 1 install

#: Install other Perl packages that this package needs
installdeps:
	perl Build --makefile_env_macros 1 installdeps

#: Make a MANIFEST file
manifest:
	perl Build --makefile_env_macros 1 manifest

#: Generate manual pages
manpages:
	perl Build --makefile_env_macros 1 manpages

ppd:
	perl Build --makefile_env_macros 1 ppd

ppmdist:
	perl Build --makefile_env_macros 1 ppmdist

prereq_report:
	perl Build --makefile_env_macros 1 prereq_report

pure_install:
	perl Build --makefile_env_macros 1 pure_install

skipcheck :
	perl Build --makefile_env_macros 1 skipcheck

#: Same as "test". "check" is the usual autoconf name

check: test

#: Same as test-unit
check-unit: test-unit

#: "check" but stop after 1st error, and add -v option
check-cautious: test-unit
	(cd t; for t in *.t; do \
	if ! prove -w $$t; then \
	   break; \
	fi; done)

#: Same as check-roundtrip
test-roundtrip: check-roundtrip

#: Run Perl's own tests (after decompilation)
check-roundtrip:
	cd t/roundtrip && perl runtests.pl

#: Run just the tests in the t/ directory
check-t: test-unit
	perl Build --makefile_env_macros 1 test --test_files t

#: Run just the unit tests
test-unit:
	perl Build --makefile_env_macros 1 test --test_files t/unit

#: Run all tests (t and scripts)
test: check-t check-roundtrip

#: Check code coverage
testcover:
	perl Build --makefile_env_macros 1 testcover

#: Remove change log: ChangeLog
rmChangeLog:
	rm ChangeLog || true

#: Create a ChangeLog from git via git log and git2cl
ChangeLog: rmChangeLog
	git log --pretty --numstat --summary | $(GIT2CL) >$@

#: Calling perl debugger (perldb) on each test
testdb:
	perl Build --makefile_env_macros 1 testdb

testpod:
	perl Build --makefile_env_macros 1 testpod

testpodcoverage:
	perl Build --makefile_env_macros 1 testpodcoverage

versioninstall:
	perl Build --makefile_env_macros 1 versioninstall

.EXPORT: INC PREFIX DESTDIR VERBINST INSTALLDIRS TEST_VERBOSE LIB UNINST INSTALL_BASE POLLUTE

.PHONY: all realclean build clean check check-t check-cautious \
        test testcover testdb testpod testpodcoverage \
        test-unit test-roundtrip
