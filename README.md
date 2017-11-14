# hell

My first version of this experiment wasn't good; I never used it.

I've been working on a more practical version based somewhat on my
work with [the jl](https://github.com/chrisdone/jl) tool which is
simply typed lambda calculus for querying JSON data structures. I
believe the same mini-language can work well for a shell.

I'm using this document as a brainstorming area and to write up observations.

## Shell is a templating language

The good thing about Sh and its descendents is that by default it is
"quoted". I.e. it produces lists of strings:

    ls -al foo

This is the list of strings:

    ["ls","-al","foo"]

The special syntactical character is ` ` (space). Other special
characters are [listed below](#special-sh-characters). Aside from
these special characters, all other text is quoted. This makes shells
a templating language.

I believe it's neccessary to preserve this property of shells.

## Shell is a free monad

This quoted descripion is fairly well described as a rough ADT:

``` haskell
data Shell
  = Command [String] -- ls -al foo
  | Pipe [Command] -- |
  | Sequence [Shell] -- ;
  | Redirect Shell FilePath -- ls > foo.txt
  | Background Shell -- ls &
  | Substitution Shell (String -> Shell) -- $(...) or `...`
```

Substitution is where the shell gets its `join` operator, or `>>=`, in
which it can make decisions. Before that, it's more of an arrow.

``` shell
nc $(docker-machine ip server)
```

is similar to

``` haskell
Command ["docker-machine","ip","server"]
  >>= \sub1 ->
    Command ["nc", sub1]
```

The syntax in sh `ls x.*` is a runtime expansion depending on the
contents of the directory. It might expand to `ls x.txt x.foo`
etc. This logic can be handled by an additional `MatchSubstitution`
construtor:

``` haskell
MatchSubstitution [Pattern] ([String] -> Shell)

data Pattern = Plain String | Wild | AnyChar
```

## Special sh characters

Aside from the below, there is also `if`, `do`, `while`, etc.

Lexical helpers

* `#` comment
*  `"` quote
* `'` quote
* <code>\\</code> char quote

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
