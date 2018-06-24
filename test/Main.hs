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
             (lexed "var letvar" [LowerWordToken "var", LowerWordToken "letvar"])
           it "Number" (lexed "123" [NumberToken 123])
           it
             "Symbols"
             (lexed
                "=[](),"
                [ EqualsToken
                , OpenBracketToken
                , CloseBracketToken
                , OpenParenToken
                , CloseParenToken
                , CommaToken
                ])
           it
             "Quotation"
             (lexed
                "{ foo; bar }"
                [ QuoteBeginToken
                , QuotedToken "foo"
                , SemiToken
                , QuotedToken "bar"
                , QuoteEndToken
                ])
           it
             "Quotation with variable splice"
             (lexed
                "{ foo; $bar }"
                [ QuoteBeginToken
                , QuotedToken "foo"
                , SemiToken
                , SpliceVarToken "bar"
                , QuoteEndToken
                ])
           it
             "Quotation with code splice"
             (lexed
                "{ foo; ${bar} }"
                [ QuoteBeginToken
                , QuotedToken "foo"
                , SemiToken
                , SpliceBeginToken
                , LowerWordToken "bar"
                , SpliceEndToken
                , QuoteEndToken
                ])
           it
             "Comments"
             (lexed
                "foo#bar\nmu"
                [LowerWordToken "foo", CommentToken "bar", LowerWordToken "mu"])))
  where
    lexed i y =
      case Mega.runParser (lexUnquoted <* Mega.eof) "" i of
        Left e -> error (Mega.parseErrorPretty' i e)
        Right x -> shouldBe (map locatedThing (toList x)) y
