# hell

My first version of this experiment wasn't good; I never used it.

I've been working on a more practical version based somewhat on my
work with [the jl](https://github.com/chrisdone/jl) tool which is
simply typed lambda calculus for querying JSON data structures. I
believe the same mini-language can work well for a shell.

Watch this space for updates.

## Observations

The good thing about Sh and its descendents is that by default it is
"quoted". I.e. it produces lists of strings:

    ls -al foo

This is the list of strings:

    ["ls","-al","foo"]

The special syntactical character is ` ` (space). Other special
characters are listed below. Aside from these special characters, all
other text is quoted.

I believe it's neccessary to preserve this quote-by-default behavior
of shells.

## Special sh characters

Lexical helpers

* `#` comment
*  `"` quote
* `'` quote
* <code>\</code> char quote

Variables

* `$` variable
* `${...}` variable
* `$<foo>` various globals
* `%` job number

Process control

* `|` pipe
* `&` background job
*  <code>`</code> command substitute
* `;` command separator
* `$(..)` command substitute
* `( .. ; ..)` subshell
* `{ .. ; ..}` sequence
* `>`, `<`, `<<`, `>>` redirect IO

Matching

* `*` match 0+ charactes
* `?` match character
* `~` home directory (and `~foo`)

Arithmetic

* `((...))`

Misc

`!`
