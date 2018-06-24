{-# LANGUAGE LambdaCase #-}

-- | Parser for the Hell language.

module Hell.Parser where

import           Data.ByteString (ByteString)
import           Data.Sequence (Seq)
import           Data.Void
import           Hell.Lexer
import           Hell.Types
import qualified Text.Megaparsec as Mega

-- | Parse (Located Token)s into an AST.
type  Parser = Mega.Parsec Void (Seq (Located Token))

data Expression = VariableExpression !ByteString

parseUnquotedByteString :: FilePath -> ByteString -> Either String (Located ByteString)
parseUnquotedByteString fp bs =
  case lexUnquotedByteString fp bs of
    Left e -> Left e
    Right toks ->
      case parseUnquoted fp toks of
        Right k -> pure k
        Left e -> Left "parse error"

parseUnquoted :: FilePath -> Seq (Located Token) -> Either (Mega.ParseError (Located Token) Void) (Located ByteString)
parseUnquoted fp toks = Mega.parse lowerWordParser fp toks

lowerWordParser :: Parser (Located ByteString)
lowerWordParser =
  Mega.token
    (\case
       l@(Located {locatedThing = LowerWordToken token}) -> Right (fmap (const token) l)
       _ -> Left (Nothing, mempty))
    Nothing
