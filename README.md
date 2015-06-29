EBNF: Extended Backus-Naur Form
-------------------------------
This is a hobby project from the early 1990s that got out of hand.  It
provides LL(1) parser generation for Icon, C, and Standard ML.  It
does some SLR(1) for Icon only, and it will also rewrite grammars for
yacc or mlyacc.

The main advantage over other parser generators is that it uses
Wirth's EBNF notation, so sequence, optional, and alternation (with
parenthetical brackets) are all built in.  Also, you can use a
reserved word just by quoting it---EBNF takes care of the rest.

The main advantage over something like parsing combinators is that
EBNF computes first and follow sets, and given something like the
`-picky` option, will error exit if it detects an ambiguity.


Sample command
--------------

````
BLKSIZE=4000000 STRSIZE=4000000 ./ebnf -slr grammar.gr > grammar.icn
````

