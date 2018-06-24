{-# LANGUAGE LambdaCase #-}

-- | Parser for the Hell language.

module Hell.Parser where

import           Data.ByteString (ByteString)
import           Data.Sequence (Seq)
import           Data.Void
import           Hell.Types
import qualified Text.Megaparsec as Mega

-- | Parse (Located Token)s into an AST.
type  Parser = Mega.Parsec Void (Seq (Located Token))

data Expression = VariableExpression !ByteString

unquotedParser :: FilePath -> Seq (Located Token) -> Either (Mega.ParseError (Located Token) ()) ()
unquotedParser fp toks = Mega.parse (pure ()) fp toks

lowerWordParser :: Parser (Located ByteString)
lowerWordParser =
  Mega.token
    (\case
       l@(Located {locatedThing = LowerWordToken token}) -> Right (fmap (const token) l)
       _ -> Left (Nothing, mempty))
    Nothing
