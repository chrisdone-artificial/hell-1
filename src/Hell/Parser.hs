-- | Parser for the Hell language.

module Hell.Parser where

import           Data.ByteString (ByteString)
import           Data.Sequence (Seq)
import           Data.Void
import           Hell.Types
import qualified Text.Megaparsec as Mega

-- | Parse LTokens into an AST.
type  Parser = Mega.Parsec Void (Seq LToken)

data Expression = VariableExpression !ByteString

unquotedParser :: FilePath -> Seq LToken -> Either (Mega.ParseError LToken ()) ()
unquotedParser fp toks = Mega.parse (pure ()) fp toks

lowerWordParser :: Parser LToken
lowerWordParser = undefined
