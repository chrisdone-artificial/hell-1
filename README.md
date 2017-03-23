# hell

Idea stage, no implementation.

# Syntactical elements

## Variables

    $x :: a

    $y :: a

## Pipe

    x | y is a pipe
    x > y sink/source
    x >> y append

## Comment

    # foo

## Self-evaluating symbols

    -flag :: String
    --posix flag :: String
    -flag=foo :: String

## Quoted strings

    "escaping quote" :: String
    "multiline
    quotes" :: String
    "${x}$e" :: String

## Numbers

    123 :: Int
    4.5 :: Double

# Everything is a pipe

    type map (i -> o) -> Pipe i o

# Examples

    let $x = 1 , $y = 2 in $x * $y

    grep -v ^x$ | $map ($x => $x * $x)
