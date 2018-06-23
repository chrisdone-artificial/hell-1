{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the Hell language.

module Hell.Lexer
  ( lexFile
  , lexQuoted
  , lexUnquoted
  ) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Char
import           Data.Sequence (Seq(), (<|), (|>))
import           Data.Void
import           Data.Word
import           Hell.Types
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Byte as Mega
import           Text.Megaparsec.Error

-- | Lex bytes into a series of LTokens.
type Lexer = Mega.Parsec Void ByteString

lexFile :: FilePath -> IO (Seq LToken)
lexFile fp = do
  bs <- S.readFile fp
  (case Mega.runParser (lexUnquoted <* Mega.eof) "" bs of
     Left e -> error (parseErrorPretty' bs e)
     Right x -> pure x)

lexQuoted :: Lexer (Seq LToken)
lexQuoted =
  fmap
    mconcat
    (Mega.some
       (fmap pure lexComment <|> unquoted <|> var <|> byteString <|> string))
  where
    unquoted = do
      begin <- located (SpliceBegin <$ Mega.string "${")
      inner <- lexUnquoted
      end <- located (SpliceEnd <$ Mega.string "}")
      pure (begin <| (inner |> end))
    var =
      fmap
        pure
        (located
           (SpliceVar <$>
            (Mega.char (c2w '$') *>
             Mega.takeWhile1P Nothing ((\c -> isAlphaNum c) . w2c))))
    byteString =
      fmap
        pure
        (located (Quoted <$> Mega.takeWhile1P Nothing (not . boundary . w2c)))
    string = fmap pure lexString
    boundary c = c == '"' || c == '$' || c == '}' || c == '#'

lexUnquoted :: Lexer (Seq LToken)
lexUnquoted =
  fmap
    mconcat
    (Mega.some
       (fmap pure lexComment <|> quoted <|> byteString <|> fmap pure lexString))
  where
    quoted = do
      begin <- located (QuoteBegin <$ Mega.string "{")
      inner <- lexQuoted
      end <- located (QuoteEnd <$ Mega.string "}")
      pure (begin <| (inner |> end))
    byteString =
      fmap pure (located (Unquoted <$> Mega.takeWhile1P Nothing (not . boundary . w2c)))
    boundary c = c == '"' || c == '}' || c == '{' || c == '#'

lexComment :: Lexer LToken
lexComment =
  located
    (Comment <$>
     (Mega.char (c2w '#') *>
      Mega.takeWhileP Nothing (not . (\c -> c == '\r' || c == '\n') . w2c)))

lexString :: Lexer LToken
lexString =
  located
    (StringLiteral <$>
     (Mega.char (c2w '"') *> innerString <* Mega.char (c2w '"')))
  where
    innerString = Mega.takeWhileP Nothing (/= c2w '"')

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral

located :: Mega.MonadParsec e s m => m Token -> m LToken
located m = do
  start <- Mega.getPosition
  v <- m
  end <- Mega.getPosition
  pure (LToken start end v)
