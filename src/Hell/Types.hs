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
import           Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq
import           Data.Text (Text)
import           Prelude hiding (error)
import           System.Exit (ExitCode)
import qualified Text.Megaparsec as Mega

-- | A shell pipeline.
data Shell i o r where
  Command :: Text -> [Text] -> Shell ByteString ByteString ExitCode
  -- ^ A command reads/writes and returns an exit code.

  Pipe :: Shell i ByteString _a -> Shell ByteString o r -> Shell i o r
  -- ^ Piped commands certainly read/write to eachother, but no
  -- constraints other than that.

  Sequence :: Shell i _o _a -> Shell i o r -> Shell i o r
  -- ^ A sequence of commands doesn't necessarily write anything.

  Redirect :: Shell i ByteString r -> Redirect -> Shell i o r
  -- ^ A redirect may read, must accept a 'Shell' which writes, but
  -- produces '()' output in its final type, instead writing the
  -- output to a file.

  Background :: Shell i o r -> Shell i o ThreadId
  -- ^ Launch a thread process in the background. Returns the thread
  -- id, produces no pipeable output.

  Substitution
    :: Shell _i ByteString _o
    -> (ByteString -> Shell i o r)
    -> Shell i o r
  -- ^ Run a shell producing an output, consuming no input, and return
  -- value unused. Feed its output into the continuation.

  Conduit :: ConduitT ByteString ByteString IO () -> Shell ByteString ByteString ()
  -- ^ Run a pure Haskell conduit.

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
  | UnquotedToken !ByteString
  | StringLiteralToken !ByteString
  | CommentToken !ByteString
  | LowerWordToken !ByteString
  | EqualsToken
  | LetToken
  | OpenBracketToken
  | CloseBracketToken
  | OpenParenToken
  | CloseParenToken
  | NumberToken !Integer
  | CommaToken
  | SemiToken
  | AmpersandToken
  | WhereToken
  | GreaterToken
  | DoubleGreaterToken
  | BarToken
  deriving (Show, Eq, Ord, Data)

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
