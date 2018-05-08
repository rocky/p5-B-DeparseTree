Change Version
--------------

Check `$VERSION` in `lib/B/DeparseTree.pm`
run

	perl ./Build.PL

to get new version in.

Build tarball:
--------------

    make check
    make distcheck

    BDT_VERSION=2.1.1 # or whatever it says in lib/B/DeparseTree.pm
    git commit -m"Get ready for release $BDT_VERSION" .

Update ChangeLog:
-----------------

    make ChangeLog

Update NEWS
------------

    git commit --amend
    git push

    make dist

Login to PAUSE:

   https://pause.perl.org/

Upload a file to CPAN on left-hand-side

upload `/src/external-vcs/Perl-Devel-Trepan-${BDT_VERSION}.tar.gz`

	echo git tag release-${BDT_VERSION}
	git tag release-${BDT_VERSION}
	git push --tags
	update version in lib/B/DeparseTree.pm add _001.
