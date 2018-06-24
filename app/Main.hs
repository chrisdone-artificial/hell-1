{-# LANGUAGE OverloadedStrings #-}

-- | Main Hell executable.

module Main where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Data
import           Data.Foldable
import           Data.Monoid
import           Data.Sequence (Seq)
import           Hell.Lexer
import           Hell.Types
import           Options.Applicative.Simple
import qualified Text.Megaparsec as Mega

data Lex
  = LexQuotedEmacs
  | LexUnquotedEmacs
  deriving (Eq, Show)

data Config = Config
  { configLex :: Maybe Lex
  } deriving (Show)

main :: IO ()
main = do
  (opts, ()) <-
    simpleOptions
      "0"
      "Hell"
      "A shell"
      (Config <$>
       (flag
          Nothing
          (Just LexQuotedEmacs)
          (help "Lex stdin as shell commands and output token info for Emacs" <>
           long "lex-commands-emacs") <|>
        flag
          Nothing
          (Just LexUnquotedEmacs)
          (help "Lex stdin as pure code and output token info for Emacs" <>
           long "lex-pure-emacs")))
      empty
  case configLex opts of
    Nothing -> pure ()
    Just LexQuotedEmacs ->
      S.interact (tokensToEmacs . lexQuotedByteString "<interactive>")
    Just LexUnquotedEmacs ->
      S.interact (tokensToEmacs . lexUnquotedByteString "<interactive>")

tokensToEmacs :: Either String (Seq (Located Token)) -> ByteString
tokensToEmacs xs =
  "(" <> S.intercalate "\n " (map fromToken (either (const []) toList xs)) <>
  ")\n"
  where
    fromToken located =
      "(" <> S.intercalate " " (map (\(k, v) -> ":" <> k <> " " <> v) keys) <>
      ")"
      where
        keys =
          [ ( "start-line"
            , S8.pack (show (Mega.unPos (Mega.sourceLine (locatedStart located)))))
          , ( "start-column"
            , S8.pack
                (show (Mega.unPos (Mega.sourceColumn (locatedStart located)))))
          , ( "end-line"
            , S8.pack (show (Mega.unPos (Mega.sourceLine (locatedEnd located)))))
          , ( "end-column"
            , S8.pack (show (Mega.unPos (Mega.sourceColumn (locatedEnd located)))))
          , ("type", S8.pack (show (toConstr (locatedThing located))))
          ]
