# Standalone testing programs

Here we have some more standalone tests to assist in deparsing.

## runtests.pl

`runtests.pl` will decompile all of the programs in the `base` and `opbasic` directories
and decompile each file in there to the corresponding location in `tmp` under this directory.

## frag.pl

`frag.pl` shows information about all B::DeparseTree nodes so you can check that the tree is build properly and what source-code fragments would be associated with each node.

If you pass an argument is it assumed to be a path to a Perl module
relative to this directory.  The default file is `bug.pm`. That module
is assumed to have a subroutine called "bug" which is then decompiled
after the module file. it is `required`.

To start you off, you can copy `bug-sample.pm` to `bug.pm` and modify from there.
