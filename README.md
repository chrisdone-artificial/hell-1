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
characters are listed below. Aside from
these special characters, all other text is quoted. This makes shells
a templating language.

I believe it's neccessary to preserve this property of shells.

## Shell is a free monad

This quoted descripion is fairly well described as a rough ADT:

``` haskell
data Shell
  = Command [String] -- ls -al foo
  | Pipe [Shell] -- |
  | Sequence [Shell] -- ;
  | Redirect Shell FilePath -- ls > foo.txt
  | Background Shell -- ls &
  | Substitution Shell (String -> Shell) -- $(...) or `...`
```

> **Side note** Actually, there are some questions here. `Pipe` can't really pipe
`ls > x.txt` with `cat` because the output has been redirected to
`x.txt`. Should we disallow that in the ADT? Or perhaps all `Shell`
can be piped and if it's a redirected then the output is simply empty,
because stdout is closed. The same applies to background `ls&` which
doesn't output to stdout but rather a new pipe output.
>
> An example type might be:
>
> ``` haskell
> data In
> data Out
> data None
>
> data Shell i o a where
>   Command :: [String] -> Shell In Out a
>   Pipe :: Shell i Out a -> Shell In o a -> Shell i o a
>   Sequence :: Shell i _o a -> Shell _i o a -> Shell i o a
>   Redirect :: Shell i Out a -> FilePath -> Shell i None a
>   Background :: Shell i _o a -> Shell i None a
>   Substitution :: Shell None Out a -> (String -> Shell i o a) -> Shell i o a
> ```
>

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

Special commands like `cd`, `pwd`, `time` would be additional
constructors.

# Syntactical analysis of sh/bash

Special reserved words in bash: ! case  do done elif else esac fi for function if in select then until while { } time [[ ]]

Special commands in bash: cd, pwd, eval, time

Not mentioned in lists is: `=` which defines variables

Special sh characters:

* Lexical helpers: `#` comment,  `"` quote, `'` quote, <code>\\</code> char quote
* Variables: `$` variable, `${...}` variable, `$<foo>` various
  globals, `%` job number, , `~` home directory (and `~foo`)
* Process control: `|` pipe, `&` background job,  <code>\`</code> command substitute, `;` command separator, `$(..)` command substitute, `( .. ; ..)` subshell, `{ .. ; ..}` sequence, `>`, `<`, `<<`, `>>` redirect IO
* Matching: `*` match 0+ characters, `?` match character
* Arithmetic: `((...))`
* Misc: `!`

Short version:

    !"$&'()*,:;<=>?@[\]^`{|}
