# Introduction

As someone who make a lot of mistakes, I've long wanted to improve the
precision of debugging and error reporting. Through Perlmonks, I was
directed upon the idea of using the OP address as means to get more
detailed information of where the program is. I was also pointed to
`B::Deparse`.

At the time, I was elated. Perl5 uses a tree interpreter to run your
Perl script. So what a decompiler does here to recreate the source
code, and in in particular what `B::Deparse` does, is walk the `B::OP`
tree, building up strings as it traverses. If you start from the root
of the tree, you get everything. If instead you start from some other
node, you get the text that this portion is responsible for. So I
thought all I need to do is augment `B::Deparse` a little bit. True,
but...


# Deparse problems

The biggest issue with `B::Deparse` is it is one 6.5K file. There are
two tests for it, but stored in different directories. The sheer
*number* of tests are huge too - hundreds of tests in one, and a
couple thousand in the other. The reason for the large number of tests
in the latter test is a result of perl having so many builtin
functions - over 100 of them. Each of these is an opcode; and each
function can be tried several times in different ways.

There there is such extensive testing is great, but given Perl
testing's default behavior of running all tests unconditionally, the
slightest error meant that I'd see a spew of error of hundreds to
thousands of line. (Does `prove` or `Test::More` have simple way to
stop on the first failure?)

So it was important to get tests under control, I mean starting with
some quick little unit tests, and bailing if those fail. Unit tests
and B::Deparse? Now there's a concept!

When a program is big and monolithic, it is often not modular. When it
is not modular, it's hard to test it. That is most likely why both
tests for `B::Deparse` that require a lot of setup. The tests can also
be frail if the formatting changes slightly. A formatting change to
the setup boilerplate will cause all tests to fail.

How we do better? I've mentioned unit tests. Later I'll describe
something else that elimintates the fragilness of testing against a
particular kind of formatting.

# Testing

When `B::Deparse fails`, it's hard to understand what it is talking about:

Here is an example:

```
#   Failed test 'Constants in a block'
#   at t/20-deparse-small.t line 114.
# {
#      {
#         '??';
#         2
# }
# }'
#     doesn't match '(?^:^\s*\{\s*\s+\{\s+\'\?\?\?\'\;\s+2\s+)'
```

Is this clear to you what's wrong? And line 114 is the line number of
code that is reading data. What you really want to know is the line
number of the data that it is reading. The best it does is give
'Constants in a block' so you can search for that.

By the way, I have split out the test data from the code so that I can
reuse the code for different Perl versions.

The way I currently address the above is to show in addition to the
above message, a diff of two compared texts:

```
# ***************
# *** 1,7 ****
#
#   {
# !      {
# !         '??';
# !         2
# ! }
# ! }
# \ No newline at end of file
# --- 1,4 ----
#
#   {
# !     '???';
# !     2
#
```

I can easily ignore remove differences in what spaces to see what's wrong.

Shortly though what I'll be adding is a way to write to a file just
that test case that failed, so that it is isolated and can be tested
by itself.

-- Runing t/base/cond.t etc

# The need for modularity

Given that B::Deparse isn't all modular, and it is a single file, the
most expedient way to extend its behavior was to copy the file and
modify it. While expedient this now runs into a problem once you want
to a put out package for multiple versions of Perl.

Yes, you can copy those other versions as well, but instead of one
6.5K mess you now have upwards of five 6.5K messes. Clearly this
doesn't scale if you want to support more Perl versions.

Lack modularity, then, is less viable of an option once you try to
decouple B::Deparse from Perl.

You could, and I have done, pour over diffs over released versions of
the 6.5K file. This is not fun. Mentally you need to associate groups
of changed sections with a logical change. To get the logical change
structure you'd need to look at changes by version-control diffs, and
if the changes aren't squashed (which is most certainly the case to
some extent), then this is tedious too.

And *after* that, then you need to separate each changes into one of 3
categories:

1. Bug fixes that you want from the newer version to take effect in
   the older version
2. Nonfunctional, but stylistic changes
3. Changes that reflect real changes between versions and so the
   two sets of code need be kept separately

So again, in sum there's really no choice but to make the code more
modular. Aside from the modularity benefits, there is an additional
the benefit when this is done of being able to see in code what has
changed over the revisions of Perl.

So most of my time in working with B::Deparse has been to modularize
it if not just to sort out code into common code and version-specific
changes.



# This method is the inner loop.  Rocky's comment with respect to: so
# try to keep it simple Most normal programs really aren't that
# big. Yeah I know there are a couple of big pigs like the B::Deparse
# code itself. The perl5 debugger comes to mind too. But what's the
# likelihood of anyone wanting to decompile all of this? If someone
# can make a case for a reason high-speed decompilation is vital, I'd
# like to hear it.


# Table-driven opcodes

With regard to the table format:

In trying to use and modularize this code, I see there is
a lot of repetition in subroutine parsing routines.

Compare this:


sub pp_die { listop(@_, "die") }
sub pp_each { unop(@_, "each") }
sub pp_egrent { baseop(@_, "endgrent") }

with

    'die'        => 'listop',
    'each'       => 'unop',
    'egrent'     => ['baseop', 'endgrent'],

Was it obvious to you when looking at the subroutine call, that the
name "egrent" got converted to "endgrent" which didn't happen in other
shown entries?

Also, it is easier to customize the entries as per Perl version nees.
In some versions, some ops we need to surround an the text from an
operation with a "my", while in other versions that is not the
case. It is clearer and simpler just to change table entries than it
is to muck with OO lookup or doing subroutine assignments.

# Format-spec driven fragment creation

Conceptially what B::Deparse does is conceptually simple: it walks the
optree building and combining string fragments at a parent node from
its children. When you get to finish at the top node after walking the
entire string you have the final resulting program.

However understanding what goes on inside a given node, is very
haphazard. So if you have a bug somewhere, figuring out where there
was a problem and why is difficult.

This kind of thing you may have already encountered in a different
guise, and a good solution to that problem applies here. Imagine you
are trying to create a nicely formated report with lots of data
values. You can try combining strings together interspersed with calls
to conversion routines to different data items.

But instead what most people do is use some sort of format specifiers
so that in a template you get a sense of what's going on, and then
just fill in the values. And that's a good solution here.

[Show how we can simply using format specifiers]

loopop? indirop? if/else?

Currently `B::Deparse` embeds control characters into the string
`\cS`, `\cK`, and so on.

I have not found a situation where you can break things by adding these characters inside
a valid Perl program, but it still seems brittle.

Separating formatting the string is also helful to understanding how a
node got its way, and paves the way for more complex formatting descisions.


# Swapping out Node and Syntax tree for String-oriented routines.

The focus of code right now has been for handling this new feature. However I believe it would be a good
replacement for `B::Deparse` because of its modularity, and ease with which you can see what's going on.
The template specifiers assists separation in traversing code from building string.

Although we produce a full program right now, we can speed thing up by not storing the additional tree information.
For that you would use the OO aspect and replace SyntaxTree and Node with suitable stripped down replacements.
