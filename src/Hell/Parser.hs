{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}

-- | Parser for the Hell language.

module Hell.Parser
  ( parseUnquotedByteString
  , parseUnquoted
  ) where

import           CaseOf
import           Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import           Data.Sequence (Seq)
import           Data.Void
import           Hell.Lexer
import           Hell.Types
import qualified Text.Megaparsec as Mega

-- | Parse (Located Token)s into an AST.
type  Parser = Mega.Parsec Void (Seq (Located Token))

parseUnquotedByteString ::
     FilePath -> ByteString -> Either String (Located ByteString)
parseUnquotedByteString fp bs =
  case lexUnquotedByteString fp bs of
    Left e -> Left e
    Right toks ->
      case parseUnquoted fp toks of
        Right k -> pure k
        Left e -> Left (Mega.parseErrorPretty e)

parseUnquoted ::
     FilePath
  -> Seq (Located Token)
  -> Either (Mega.ParseError (Located Token) Void) (Located ByteString)
parseUnquoted fp toks = Mega.parse lowerWordParser fp toks

--------------------------------------------------------------------------------
-- Token parser combinators

spliceBeginParser :: Parser (Located ())
spliceBeginParser = expect $(maybeCaseOf 'SpliceBeginToken)

spliceEndParser :: Parser (Located ())
spliceEndParser = expect $(maybeCaseOf 'SpliceEndToken)

spliceVarParser :: Parser (Located ByteString)
spliceVarParser = expect $(maybeCaseOf 'SpliceVarToken)

quoteBeginParser :: Parser (Located ())
quoteBeginParser = expect $(maybeCaseOf 'QuoteBeginToken)

quoteEndParser :: Parser (Located ())
quoteEndParser = expect $(maybeCaseOf 'QuoteEndToken)

quotedParser :: Parser (Located ByteString)
quotedParser = expect $(maybeCaseOf 'QuotedToken)

subBeginParser :: Parser (Located ())
subBeginParser = expect $(maybeCaseOf 'SubBeginToken)

subEndParser :: Parser (Located ())
subEndParser = expect $(maybeCaseOf 'SubEndToken)

stringLiteralParser :: Parser (Located ByteString)
stringLiteralParser = expect $(maybeCaseOf 'StringLiteralToken)

commentParser :: Parser (Located ByteString)
commentParser = expect $(maybeCaseOf 'CommentToken)

lowerWordParser :: Parser (Located ByteString)
lowerWordParser = expect $(maybeCaseOf 'LowerWordToken)

equalsParser :: Parser (Located ())
equalsParser = expect $(maybeCaseOf 'EqualsToken)

openBracketParser :: Parser (Located ())
openBracketParser = expect $(maybeCaseOf 'OpenBracketToken)

closeBracketParser :: Parser (Located ())
closeBracketParser = expect $(maybeCaseOf 'CloseBracketToken)

openParenParser :: Parser (Located ())
openParenParser = expect $(maybeCaseOf 'OpenParenToken)

closeParenParser :: Parser (Located ())
closeParenParser = expect $(maybeCaseOf 'CloseParenToken)

numberToken :: Parser (Located Integer)
numberToken = expect $(maybeCaseOf 'NumberToken)

commaParser :: Parser (Located ())
commaParser = expect $(maybeCaseOf 'CommaToken)

semiParser :: Parser (Located ())
semiParser = expect $(maybeCaseOf 'SemiToken)

ampersandParser :: Parser (Located ())
ampersandParser = expect $(maybeCaseOf 'AmpersandToken)

greaterParser :: Parser (Located ())
greaterParser = expect $(maybeCaseOf 'GreaterToken)

doubleGreaterParser :: Parser (Located ())
doubleGreaterParser = expect $(maybeCaseOf 'DoubleGreaterToken)

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
