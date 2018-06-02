Example from File::Basename

At:

``` perl
if (grep { $type eq $_ } ('MSDOS', 'DOS', 'MSWin32', 'Epoc')) {
         -
```

we are at a pushmark which is an artificial op. I've associated it with `{'.
Note that `$_` isn't set yet. When I step, I get

``` perl
if (grep { $type eq $_ } ('MSDOS', 'DOS', 'MSWin32', 'Epoc')) {
            ----
```

so `$_` is set and you an query that. Note that `step+` (step until a different line) gets
you out of the loop.

- - - -

In `fib.pl` you can see the specific call change from `fib($x - 1)` to `fib($x - 2)`, but
this wasn't as simple as it may seem. When you go up the stack in a call for `fib($x -1)`
you get:

```
    #6:     fib($x-1) + fib($x-2);
            COP (0x5598f3e9eb28) dbstate
            BINOP (0x5598f3e9eb88) add [6]
                UNOP (0x5598f3e9edd8) entersub [3]
                    UNOP (0x5598f19f4870) null [157]
                        OP (0x5598f19f4820) pushmark
                        BINOP (0x5598f19f48b8) subtract [2]
                            OP (0x5598f19f4980) padsv [1]
                            SVOP (0x5598f19f4900) const  IV (0x5598f19eca98) 1
                        UNOP (0x5598f19f4940) null [16]
                            SVOP (0x5598f19f49c0) gv  GV (0x5598f19ecaf8) *fib
                UNOP (0x5598f3e9ebd0) entersub [5]
                    UNOP (0x5598f3e9ec48) null [157]
=>                      OP (0x5598f3e9ec10) pushmark
                        BINOP (0x5598f3e9ec90) subtract [4]
                            OP (0x5598f3e9ed58) padsv [1]
                            SVOP (0x5598f3e9ecd8) const  IV (0x5598f5a04678) 2
                        UNOP (0x5598f3e9ed18) null [16]
                            SVOP (0x5598f3e9ed98) gv  GV (0x5598f19ecaf8) *fib
```

the thing to note is that the pc has advanced past the point of the
call so when we return we are about to do a subtract. Therefore we
need to find the previous operation in the tree - it is a static
property. And from the above it might not be so obvious. You go up a couple
of parents (past null to entersub) and from the entersub you can go back
in the list to the first entersub.

Now consider what happens when we are from the second `fib` call, `fib($x-2)`:

```
    #6:     fib($x-1) + fib($x-2);
            COP (0x5598f3e9eb28) dbstate
=>          BINOP (0x5598f3e9eb88) add [6]
                UNOP (0x5598f3e9edd8) entersub [3]
                    UNOP (0x5598f19f4870) null [157]
                        OP (0x5598f19f4820) pushmark
                        BINOP (0x5598f19f48b8) subtract [2]
                            OP (0x5598f19f4980) padsv [1]
                            SVOP (0x5598f19f4900) const  IV (0x5598f19eca98) 1
                        UNOP (0x5598f19f4940) null [16]
                            SVOP (0x5598f19f49c0) gv  GV (0x5598f19ecaf8) *fib
                UNOP (0x5598f3e9ebd0) entersub [5]
                    UNOP (0x5598f3e9ec48) null [157]
                        OP (0x5598f3e9ec10) pushmark
                        BINOP (0x5598f3e9ec90) subtract [4]
                            OP (0x5598f3e9ed58) padsv [1]
                            SVOP (0x5598f3e9ecd8) const  IV (0x5598f5a04678) 2
                        UNOP (0x5598f3e9ed18) null [16]
                            SVOP (0x5598f3e9ed98) gv  GV (0x5598f19ecaf8) *fib
```

Again we are at _about_ to do an `add` when we return. Notice though that the previous
instruction is in fact the right-hand expression of this binary operation.

`B::DeparseTree` negotiates this by storing "prev" entries in the tree.


- - - -

Note what's up with ~ vs -:

When we can find the child node as a placeholder in its parent, the underline is '-'.
However when we can't, but we have found it unambigously by doing a string search of
one text within another we use ~. We also use a different symbol for artificially
created position |.
