{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the Hell language.

module Hell.Lexer where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import qualified Data.Attoparsec.ByteString as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Char

data Quoted
  = QuotedSplice ![Unquoted]
  | QuotedByteString !ByteString
  | QuotedString !ByteString
  | QuotedSpliceWord !ByteString
  | QuotedComment !ByteString
  deriving (Show)

data Unquoted
  = UnquotedSplice ![Quoted]
  | UnquotedByteString !ByteString
  | UnquotedString !ByteString
  | UnquotedSpliceWord !ByteString
  | UnquotedComment !ByteString
  deriving (Show)

lexFile :: FilePath -> IO [Unquoted]
lexFile fp = do
  bytes <- S.readFile fp
  case Atto8.parseOnly (lexUnquoted <* Atto.endOfInput) bytes of
    Left e -> error e
    Right x -> pure x

lexQuoted :: Atto8.Parser [Quoted]
lexQuoted = many (comment <|> unquoted <|> var <|> byteString <|> string)
  where
    comment =
      QuotedComment <$>
      (Atto8.char '#' *> Atto8.takeTill (\c -> c == '\r' || c == '\n'))
    unquoted =
      QuotedSplice <$> (Atto8.string "${" *> lexUnquoted <* Atto8.char '}')
    var =
      QuotedSpliceWord <$>
      (Atto8.char '$' *> Atto8.takeWhile1 (\c -> isAlphaNum c))
    byteString = QuotedByteString <$> Atto8.takeWhile1 (not . boundary)
    string = QuotedString <$> lexString
    boundary c = c == '"' || c == '$' || c == '}' || c == '#'

lexUnquoted :: Atto8.Parser [Unquoted]
lexUnquoted = many (comment <|> quoted <|> byteString <|> string)
  where
    comment =
      UnquotedComment <$>
      (Atto8.char '#' *> Atto8.takeTill (\c -> c == '\r' || c == '\n'))
    quoted =
      UnquotedSplice <$> (Atto8.string "{" *> lexQuoted <* Atto8.char '}')
    byteString = UnquotedByteString <$> Atto8.takeWhile1 (not . boundary)
    string = UnquotedString <$> lexString
    boundary c = c == '"' || c == '}' || c == '{' || c == '#'

lexString :: Atto8.Parser ByteString
lexString = Atto8.char '"' *> innerString <* Atto8.char '"'
  where
    innerString = do
      (_, (backslash, string)) <-
        Atto.runScanner
          (False, id)
          (\(backslash, string) c ->
             if backslash
               then pure (False, string . (c :))
               else case toEnum (fromIntegral c) of
                      '\\' -> pure (True, string)
                      '"' -> Nothing
                      _ -> pure (False, string . (c :)))
      if backslash
        then fail "missing escape character after \\"
        else pure (S.pack (string []))
