{-# LANGUAGE OverloadedStrings #-}

-- | Main Hell executable.

module Main where

import Data.Monoid
import Options.Applicative.Simple

data Lex
  = LexEmacs
  | LexJson
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
      (flag
         Nothing
         (Just LexEmacs)
         (help "Lex stdin and output token info for Emacs" <> long "lex-emacs") <|>
       flag
         Nothing
         (Just LexJson)
         (help "Lex stdin and output JSON for an editor to read" <>
          long "lex-json"))
      empty
  print opts
