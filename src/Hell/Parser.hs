{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- | Parser for the Hell language.

module Hell.Parser
  ( parseQuotedByteString
  , parseQuoted
  ) where

import           CaseOf
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Foldable
import qualified Data.List.NonEmpty as NE
import           Data.Sequence (Seq())
import qualified Data.Text.Encoding as T
import           Data.Void
import           Hell.Lexer
import           Hell.Types
import           System.Exit
import qualified Text.Megaparsec as Mega

-- | Parse (Located Token)s into an AST.
type Parser = Mega.Parsec Void (Seq (Located Token))

--------------------------------------------------------------------------------
-- Quoted code parsers

-- | Parse a quoted string like @ls -alh@.
parseQuotedByteString ::
     FilePath -> ByteString -> Either String SomeShell
parseQuotedByteString fp bs =
  case lexQuotedByteString fp bs of
    Left e -> Left e
    Right toks ->
      case parseQuoted fp toks of
        Right k -> pure k
        Left e -> Left (Mega.parseErrorPretty e)

-- | Parse a quoted set of tokens like @ls -ali@.
parseQuoted ::
     FilePath
  -> Seq (Located Token)
  -> Either (Mega.ParseError (Located Token) Void) SomeShell
parseQuoted fp toks = Mega.parse shellParser fp toks

-- | A parser for some shell pipeline.
shellParser :: Parser SomeShell
shellParser = do
  sequenceParser

-- | Parser for a sequence of commands e.g. @x | y | z; second; third@.
sequenceParser :: Parser SomeShell
sequenceParser = do
  x <- pipeParser
  sequenced x <|> backgrounded x <|> pure (SomeShell x)
  where
    sequenced x =
      semiParser *>
      (do y <- sequenceParser
          case y of
            SomeShell y' -> pure (SomeShell (Sequence x y')))
    backgrounded x = ampersandParser *> pure (SomeShell (Background x))

-- | Parser for a pipe of commands e.g. @x | y | z@.
pipeParser :: Parser (Shell ByteString ByteString ExitCode)
pipeParser = do
  x <- commandParser
  xs <- Mega.many (barParser *> commandParser)
  pure (foldl Pipe x xs)

-- | Parser for a shell command e.g. @ls -alh@.
commandParser :: Parser (Shell ByteString ByteString ExitCode)
commandParser = do
  cmd <- quotedParser
  args <- Mega.many quotedParser
  pure
    (Command
       (T.decodeUtf8 (locatedThing cmd))
       (map (T.decodeUtf8 . locatedThing) (toList args)))

--------------------------------------------------------------------------------
-- Token parser combinators

-- spliceBeginParser :: Parser (Located ())
-- spliceBeginParser = expect $(maybeCaseOf 'SpliceBeginToken)

-- spliceEndParser :: Parser (Located ())
-- spliceEndParser = expect $(maybeCaseOf 'SpliceEndToken)

-- spliceVarParser :: Parser (Located ByteString)
-- spliceVarParser = expect $(maybeCaseOf 'SpliceVarToken)

-- quoteBeginParser :: Parser (Located ())
-- quoteBeginParser = expect $(maybeCaseOf 'QuoteBeginToken)

-- quoteEndParser :: Parser (Located ())
-- quoteEndParser = expect $(maybeCaseOf 'QuoteEndToken)

quotedParser :: Parser (Located ByteString)
quotedParser = expect $(maybeCaseOf 'QuotedToken)

-- subBeginParser :: Parser (Located ())
-- subBeginParser = expect $(maybeCaseOf 'SubBeginToken)

-- subEndParser :: Parser (Located ())
-- subEndParser = expect $(maybeCaseOf 'SubEndToken)

-- stringLiteralParser :: Parser (Located ByteString)
-- stringLiteralParser = expect $(maybeCaseOf 'StringLiteralToken)

-- commentParser :: Parser (Located ByteString)
-- commentParser = expect $(maybeCaseOf 'CommentToken)

-- lowerWordParser :: Parser (Located ByteString)
-- lowerWordParser = expect $(maybeCaseOf 'LowerWordToken)

-- equalsParser :: Parser (Located ())
-- equalsParser = expect $(maybeCaseOf 'EqualsToken)

-- openBracketParser :: Parser (Located ())
-- openBracketParser = expect $(maybeCaseOf 'OpenBracketToken)

-- closeBracketParser :: Parser (Located ())
-- closeBracketParser = expect $(maybeCaseOf 'CloseBracketToken)

-- openParenParser :: Parser (Located ())
-- openParenParser = expect $(maybeCaseOf 'OpenParenToken)

-- closeParenParser :: Parser (Located ())
-- closeParenParser = expect $(maybeCaseOf 'CloseParenToken)

-- numberToken :: Parser (Located Integer)
-- numberToken = expect $(maybeCaseOf 'NumberToken)

-- commaParser :: Parser (Located ())
-- commaParser = expect $(maybeCaseOf 'CommaToken)

semiParser :: Parser (Located ())
semiParser = expect $(maybeCaseOf 'SemiToken)

ampersandParser :: Parser (Located ())
ampersandParser = expect $(maybeCaseOf 'AmpersandToken)

-- greaterParser :: Parser (Located ())
-- greaterParser = expect $(maybeCaseOf 'GreaterToken)

-- doubleGreaterParser :: Parser (Located ())
-- doubleGreaterParser = expect $(maybeCaseOf 'DoubleGreaterToken)

barParser :: Parser (Located ())
barParser = expect $(maybeCaseOf 'BarToken)

expect :: (Token -> Maybe a) -> Parser (Located a)
expect f =
  Mega.token
    (\case
       l@(Located {locatedThing = tok})
         | Just token <- f tok -> Right (fmap (const token) l)
       l -> Left (Just (Mega.Tokens (NE.fromList [l])), mempty))
    Nothing
