{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- | All types for the language.

module Hell.Types where

import           Control.Concurrent (ThreadId)
import           Data.ByteString (ByteString)
import           Data.Conduit (ConduitT)
import           Data.Data
import           Data.Foldable
import           Data.Monoid
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Prelude hiding (error)
import           System.Exit (ExitCode)
import qualified Text.Megaparsec as Mega

-- | A shell pipeline.
data Shell r where
  Command :: Text -> [Text] -> Shell ExitCode
  -- ^ A command reads/writes and returns an exit code.

  Pipe :: Shell _a -> Shell r -> Shell r
  -- ^ Piped commands certainly read/write to eachother, but no
  -- constraints other than that.

  Sequence :: Shell _a -> Shell r -> Shell r
  -- ^ A sequence of commands doesn't necessarily write anything.

  Redirect :: Shell r -> Redirect -> Shell r
  -- ^ A redirect may read, must accept a 'Shell' which writes, but
  -- produces '()' output in its final type, instead writing the
  -- output to a file.

  Background :: Shell r -> Shell ThreadId
  -- ^ Launch a thread process in the background. Returns the thread
  -- id, produces no pipeable output.

  Substitution :: Shell _o -> (ByteString -> Shell r) -> Shell r
  -- ^ Run a shell producing an output, consuming no input, and return
  -- value unused. Feed its output into the continuation.

  Conduit :: ConduitT ByteString ByteString IO () -> Shell ()
  -- ^ Run a pure Haskell conduit.

  ChangeDirectory :: FilePath -> Shell ExitCode
  -- ^ Change default directory of the current process.

data SomeShell =
  forall r. SomeShell (Shell r)

-- | Redirection of a process output.
data Redirect
  = StdoutTo To
  | StderrTo To

-- | Redirect process output to.
data To
  = ToStdout
  | ToStderr
  | ToFile FilePath
  | ToFileAppend FilePath

-- | A located token.
data Located l = Located
  { locatedStart :: !Mega.SourcePos
  , locatedEnd :: !Mega.SourcePos
  , locatedThing :: !l
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Lexical tokens for the Hell language.
data Token
  = SpliceBeginToken
  | SpliceEndToken
  | SpliceVarToken !ByteString
  | QuoteBeginToken
  | QuoteEndToken
  | QuotedToken !ByteString
  | SubBeginToken
  | SubEndToken
  | StringLiteralToken !ByteString
  | CommentToken !ByteString
  | LowerWordToken !ByteString
  | EqualsToken
  | OpenBracketToken
  | CloseBracketToken
  | OpenParenToken
  | CloseParenToken
  | NumberToken !Integer
  | CommaToken
  | SemiToken
  | AmpersandToken
  | GreaterToken
  | DoubleGreaterToken
  | BarToken
  deriving (Show, Eq, Ord, Data)

instance Mega.ShowToken (Located Token) where
  showTokens = unwords . map (showToken . locatedThing) . toList

showToken :: Token -> String
showToken =
  \case
    SpliceBeginToken -> quote "open splice" "${"
    SpliceEndToken -> quote "splice end" "}"
    SpliceVarToken !sv ->
      quote "variable splice" ("$" <> T.unpack (T.decodeUtf8 sv))
    QuoteBeginToken -> quote "begin quote" "{"
    QuoteEndToken -> quote "end quote" "}"
    QuotedToken !q -> quote "quoted content " (T.unpack (T.decodeUtf8 q))
    SubBeginToken -> quote "begin substitution" "$("
    SubEndToken -> quote "end substitution" ")"
    StringLiteralToken !s -> quote "string literal" (show s)
    CommentToken !c -> quote "comment" ("#" <> T.unpack (T.decodeUtf8 c))
    LowerWordToken !w -> quote "word" (T.unpack (T.decodeUtf8 w))
    EqualsToken -> quote "equals" "="
    OpenBracketToken -> quote "open bracket" "["
    CloseBracketToken -> quote "closing bracket" "]"
    OpenParenToken -> quote "opening paren" "("
    CloseParenToken -> quote "closing paren" ")"
    NumberToken !i -> quote "number" (show i)
    CommaToken -> quote "comma" ","
    SemiToken -> quote "semicolon" ";"
    AmpersandToken -> quote "ampersand" "&"
    GreaterToken -> quote "greater than" ">"
    DoubleGreaterToken -> quote "double greater than" ">>"
    BarToken -> quote "pipe" "|"
  where
    quote label thing = label ++ " ‘" ++ thing ++ "’"

-- | This instance gives support to parse LTokens with megaparsec.
instance Ord a => Mega.Stream (Seq (Located a)) where
  type Token (Seq (Located a)) = Located a
  type Tokens (Seq (Located a)) = Seq (Located a)
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = Seq.fromList
  chunkToTokens Proxy = toList
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  positionAt1 Proxy _ (Located start _ _) = start
  positionAtN Proxy pos Seq.Empty = pos
  positionAtN Proxy _ (Located start _ _ :<| _) = start
  advance1 Proxy _ _ (Located _ end _) = end
  advanceN Proxy _ pos Seq.Empty = pos
  advanceN Proxy _ _ ts =
    let Located _ end _ = last (toList ts) in end
  take1_ Seq.Empty = Nothing
  take1_ (t :<| ts) = Just (t, ts)
  takeN_ n s
    | n <= 0   = Just (mempty, s)
    | null s   = Nothing
    | otherwise = Just (Seq.splitAt n s)
  takeWhile_ = Seq.spanl
