{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the Hell language.

module Hell.Lexer where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Char
import           Data.Sequence (Seq, (<|), (|>))
import           Data.Void
import           Data.Word
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Byte as Mega
import           Text.Megaparsec.Error

data Token
  = SpliceBegin
  | SpliceEnd
  | SpliceVar !ByteString
  | QuoteBegin
  | QuoteEnd
  | Quoted !ByteString
  | Unquoted !ByteString
  | StringLiteral !ByteString
  | Comment !ByteString
  deriving (Show, Eq)

type Parser = Mega.Parsec Void ByteString

lexFile :: FilePath -> IO (Seq Token)
lexFile fp = do
  bs <- S.readFile fp
  (case Mega.runParser (lexUnquoted <* Mega.eof) "" bs of
     Left e -> error (parseErrorPretty' bs e)
     Right x -> pure x)

lexQuoted :: Parser (Seq Token)
lexQuoted =
  fmap
    mconcat
    (Mega.some (comment <|> unquoted <|> var <|> byteString <|> string))
  where
    comment =
      fmap
        pure
        (Comment <$>
         (Mega.char (c2w '#') *>
          Mega.takeWhileP Nothing (not . (\c -> c == '\r' || c == '\n') . w2c)))
    unquoted = do
      tokens <- Mega.string "${" *> lexUnquoted <* Mega.char (c2w '}')
      pure (SpliceBegin <| (tokens |> SpliceEnd))
    var =
      fmap
        pure
        (SpliceVar <$>
         (Mega.char (c2w '$') *>
          Mega.takeWhile1P Nothing ((\c -> isAlphaNum c) . w2c)))
    byteString =
      fmap pure (Quoted <$> Mega.takeWhile1P Nothing (not . boundary . w2c))
    string = fmap pure (StringLiteral <$> lexString)
    boundary c = c == '"' || c == '$' || c == '}' || c == '#'

lexUnquoted :: Parser (Seq Token)
lexUnquoted =
  fmap mconcat (Mega.some (comment <|> quoted <|> byteString <|> string))
  where
    comment =
      fmap
        pure
        (Comment <$>
         (Mega.char (c2w '#') *>
          Mega.takeWhileP Nothing (not . (\c -> c == '\r' || c == '\n') . w2c)))
    quoted = do
      tokens <- Mega.string "{" *> lexQuoted <* Mega.char (c2w '}')
      pure (QuoteBegin <| (tokens |> QuoteEnd))
    byteString =
      fmap pure (Unquoted <$> Mega.takeWhile1P Nothing (not . boundary . w2c))
    string = pure . StringLiteral <$> lexString
    boundary c = c == '"' || c == '}' || c == '{' || c == '#'

lexString :: Parser ByteString
lexString = Mega.char (c2w '"') *> innerString <* Mega.char (c2w '"')
  where
    innerString = Mega.takeWhileP Nothing (/= c2w '"')

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral
