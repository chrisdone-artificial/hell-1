{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the Hell language.

module Hell.Lexer where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Char
import           Data.Void
import           Data.Word
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Byte as Mega
import           Text.Megaparsec.Error

data Quoted
  = QuotedSplice ![Unquoted]
  | QuotedByteString !ByteString
  | QuotedString !ByteString
  | QuotedSpliceWord !ByteString
  | QuotedComment !ByteString
  deriving (Show, Eq)

data Unquoted
  = UnquotedSplice ![Quoted]
  | UnquotedByteString !ByteString
  | UnquotedString !ByteString
  | UnquotedSpliceWord !ByteString
  | UnquotedComment !ByteString
  deriving (Show, Eq)

type Parser = Mega.Parsec Void ByteString

lexFileM :: FilePath -> IO [Unquoted]
lexFileM fp = do
  bs <- S.readFile fp
  (case Mega.runParser (lexUnquotedM <* Mega.eof) "" bs of
     Left e -> error (parseErrorPretty' bs e)
     Right x -> pure x)

lexQuotedM :: Parser [Quoted]
lexQuotedM = Mega.some (comment <|> unquoted <|> var <|> byteString <|> string)
  where
    comment =
      QuotedComment <$>
      (Mega.char (c2w '#') *> Mega.takeWhileP Nothing (not . (\c -> c == '\r' || c == '\n') . w2c))
    unquoted =
      QuotedSplice <$>
      (Mega.string "${" *> lexUnquotedM <* Mega.char (c2w '}'))
    var =
      QuotedSpliceWord <$>
      (Mega.char (c2w '$') *> Mega.takeWhile1P Nothing ((\c -> isAlphaNum c).w2c))
    byteString = QuotedByteString <$> Mega.takeWhile1P Nothing (not . boundary . w2c)
    string = QuotedString <$> lexStringM
    boundary c = c == '"' || c == '$' || c == '}' || c == '#'

lexUnquotedM :: Parser [Unquoted]
lexUnquotedM = Mega.some (comment <|> quoted <|> byteString <|> string)
  where
    comment =
      UnquotedComment <$>
      (Mega.char (c2w '#') *> Mega.takeWhileP Nothing (not . (\c -> c == '\r' || c == '\n') . w2c))
    quoted =
      UnquotedSplice <$>
      (Mega.string "{" *> lexQuotedM <* Mega.char (c2w '}'))
    byteString = UnquotedByteString <$> Mega.takeWhile1P Nothing (not . boundary . w2c)
    string = UnquotedString <$> lexStringM
    boundary c = c == '"' || c == '}' || c == '{' || c == '#'

lexStringM :: Parser ByteString
lexStringM = Mega.char (c2w '"') *> innerString <* Mega.char (c2w '"')
  where
    innerString = Mega.takeWhileP Nothing (/= c2w '"')

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral
