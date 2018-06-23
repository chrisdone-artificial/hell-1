{-# LANGUAGE OverloadedStrings #-}

-- | Test suite.

module Main where

import           Data.Foldable
import           Hell.Lexer
import           Hell.Types
import           Test.Hspec
import qualified Text.Megaparsec as Mega

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = lexer

lexer :: SpecWith ()
lexer =
  describe
    "Lexer"
    (describe
       "Unquoted"
       (do it
             "Variable"
             (lexed "var letvar" [LowerWord "var", LowerWord "letvar"])
           it "Number" (lexed "123" [Number 123])
           it
             "Symbols"
             (lexed
                "=[](),"
                [ Equals
                , OpenBracket
                , CloseBracket
                , OpenParen
                , CloseParen
                , Comma
                ])
           it
             "Quotation"
             (lexed
                "{ foo; bar }"
                [QuoteBegin, Quoted "foo", Semi, Quoted "bar", QuoteEnd])
           it
             "Quotation with variable splice"
             (lexed
                "{ foo; $bar }"
                [QuoteBegin, Quoted "foo", Semi, SpliceVar "bar", QuoteEnd])
           it
             "Quotation with code splice"
             (lexed
                "{ foo; ${bar} }"
                [ QuoteBegin
                , Quoted "foo"
                , Semi
                , SpliceBegin
                , LowerWord "bar"
                , SpliceEnd
                , QuoteEnd
                ])
           it
             "Comments"
             (lexed
                "foo#bar\nmu"
                [LowerWord "foo", Comment "bar", LowerWord "mu"])))
  where
    lexed i y =
      case Mega.runParser (lexUnquoted <* Mega.eof) "" i of
        Left e -> error (Mega.parseErrorPretty' i e)
        Right x -> shouldBe (map ltokenToken (toList x)) y
