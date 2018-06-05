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

    BDT_VERSION=3.0.0 # or whatever it says in lib/B/DeparseTree.pm
    git commit -m"Get ready for release $BDT_VERSION" .

Update ChangeLog:
-----------------

    make ChangeLog

Update NEWS
------------

    emacs NEWS
    git commit --amend


Final testing
-------------

  . ./admin/test-all.sh

   git push
   make dist

Login to PAUSE:

   https://pause.perl.org/

Upload a file to CPAN on left-hand-side

upload `/src/external-vcs/github/rocky/p5-B-DeparseTree/B-DeparseTree-${BDT_VERSION}.tar.gz`

	echo git tag release-${BDT_VERSION}
	git tag release-${BDT_VERSION}
	git push --tags
	update version in lib/B/DeparseTree.pm add _001.
