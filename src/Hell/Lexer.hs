{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the Hell language.

module Hell.Lexer where

import           Control.Applicative
import           Criterion
import           Criterion.Main
import           Data.Attoparsec.ByteString ((<?>))
import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Char
import           Data.Void
import           Data.Word
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Byte as Mega

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

benchParsers :: FilePath -> IO ()
benchParsers fp = do
  bs <- S.readFile fp
  defaultMain
    [ bench
        "attoparsec"
        (whnf
           (\bs' ->
              case Atto8.parseOnly lexUnquoted bs' of
                Left e -> error e
                Right x -> x)
           bs)
    , bench
        "megaparsec"
        (whnf
           (\bs' ->
              case Mega.runParser lexUnquotedM "" bs' of
                Left e -> error (show e)
                Right x -> x)
           bs)
    ]

--------------------------------------------------------------------------------
-- Attoparsec

lexFile :: FilePath -> IO [Quoted]
lexFile fp = do
  bs <- S.readFile fp
  (case Atto8.parseOnly lexQuoted bs of
     Left e -> error e
     Right x -> pure x)

lexQuoted :: Atto8.Parser [Quoted]
lexQuoted = Atto8.many1 (comment <|> unquoted <|> var <|> byteString <|> string)
  where
    comment =
      QuotedComment <$>
      (Atto8.char '#' *> Atto8.takeTill (\c -> c == '\r' || c == '\n'))
    unquoted =
      QuotedSplice <$>
      (Atto8.string "${" *> lexUnquoted <* (Atto8.char '}' <?> "closing }"))
    var =
      QuotedSpliceWord <$>
      (Atto8.char '$' *> Atto8.takeWhile1 (\c -> isAlphaNum c))
    byteString = QuotedByteString <$> Atto8.takeWhile1 (not . boundary)
    string = QuotedString <$> lexString
    boundary c = c == '"' || c == '$' || c == '}' || c == '#'

lexUnquoted :: Atto8.Parser [Unquoted]
lexUnquoted = Atto8.many1 (comment <|> quoted <|> byteString <|> string)
  where
    comment =
      UnquotedComment <$>
      (Atto8.char '#' *> Atto8.takeTill (\c -> c == '\r' || c == '\n'))
    quoted =
      UnquotedSplice <$>
      (Atto8.string "{" *> lexQuoted <* (Atto8.char '}'))
    byteString = UnquotedByteString <$> Atto8.takeWhile1 (not . boundary)
    string = UnquotedString <$> lexString
    boundary c = c == '"' || c == '}' || c == '{' || c == '#'

lexString :: Atto8.Parser ByteString
lexString = Atto8.char '"' *> innerString <* Atto8.char '"'
  where
    innerString = Atto8.takeWhile (/= '"')
    -- disabled for comparison with megaparsec
    _innerString = do
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

--------------------------------------------------------------------------------
-- Megaparsec

type MegaParser = Mega.Parsec Void ByteString

lexFileM :: FilePath -> IO [Quoted]
lexFileM fp = do
  bs <- S.readFile fp
  (case Mega.runParser lexQuotedM "" bs of
     Left e -> error (show e)
     Right x -> pure x)

lexQuotedM :: MegaParser [Quoted]
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

lexUnquotedM :: MegaParser [Unquoted]
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

lexStringM :: MegaParser ByteString
lexStringM = Mega.char (c2w '"') *> innerString <* Mega.char (c2w '"')
  where
    innerString = Mega.takeWhileP Nothing (/= c2w '"')

c2w :: Char -> Word8
c2w = fromIntegral . fromEnum

w2c :: Word8 -> Char
w2c = toEnum . fromIntegral
