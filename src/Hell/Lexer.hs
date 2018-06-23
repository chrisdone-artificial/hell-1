{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lexer for the Hell language.

module Hell.Lexer
  ( lexFile
  , lexQuoted
  , lexUnquoted
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Char
import           Data.Monoid
import           Data.Sequence (Seq(), (<|), (|>))
import           Data.Void
import           Data.Word
import           Hell.Types
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Byte.Lexer as Lexer
import qualified Text.Megaparsec.Byte as Mega
import           Text.Megaparsec.Error

-- | Lex bytes into a series of LTokens.
type Lexer = Mega.Parsec Void ByteString

-- | Open the file strictly and lex it.
lexFile :: FilePath -> IO (Seq LToken)
lexFile fp = do
  bs <- S.readFile fp
  (case Mega.runParser (Mega.space *> lexUnquoted <* Mega.eof) "" bs of
     Left e -> error (parseErrorPretty' bs e)
     Right x -> pure x)

-- | Lex quoted code e.g. @ls -alh@.
lexQuoted :: Lexer (Seq LToken)
lexQuoted =
  fmap
    mconcat
    (Mega.some
       (Mega.choice
          [ fmap pure lexComment
          , unquoted
          , var
          , string
          , fmap pure symbol
          , quoted
          ] <*
        Mega.space))
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
    quoted =
      fmap
        pure
        (located (Quoted <$> Mega.takeWhile1P Nothing (not . boundary . w2c)))
    string = fmap pure lexString
    symbol =
      located
        (Mega.choice
           [ Comma <$ Mega.char (c2w ',')
           , Semi <$ Mega.char (c2w ';')
           , Ampersand <$ Mega.char (c2w '&')
           , DoubleGreater <$ Mega.string ">>"
           , Greater <$ Mega.char (c2w '>')
           , Bar <$ Mega.char (c2w '|')
           ])
    boundary c =
      c == '"' || c == '$' || c == '}' || c == '#' || c == ' ' || c == ';'

-- | Lex unquoted regular code e.g. @let x = 1@.
lexUnquoted :: Lexer (Seq LToken)
lexUnquoted =
  fmap
    mconcat
    (Mega.some
       (Mega.choice
          [ fmap pure lexComment
          , quoted
          , fmap pure lexString
          , fmap pure symbol
          , fmap pure number
          , fmap pure lowerWord
          ] <*
        Mega.space))
  where
    quoted = do
      begin <- located (QuoteBegin <$ Mega.string "{")
      Mega.space
      inner <- lexQuoted
      end <- located (QuoteEnd <$ Mega.string "}")
      Mega.space
      pure (begin <| (inner |> end))
    lowerWord =
      located
        (do c <- Mega.takeWhile1P Nothing (isAlpha . w2c)
            cs <- Mega.takeWhileP Nothing (isAlpha . w2c)
            pure (LowerWord (c <> cs)))
    number = located (Number <$> Lexer.decimal)
    symbol =
      located
        (Mega.choice
           [ Equals <$ Mega.char (c2w '=')
           , OpenBracket <$ Mega.char (c2w '[')
           , CloseBracket <$ Mega.char (c2w ']')
           , OpenParen <$ Mega.char (c2w '(')
           , CloseParen <$ Mega.char (c2w ')')
           , Comma <$ Mega.char (c2w ',')
           ])

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
