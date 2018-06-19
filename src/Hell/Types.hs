{-# LANGUAGE GADTs #-}

-- | All types for the language.

module Hell.Types where

import Control.Concurrent (ThreadId)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT)
import Data.Text (Text)
import Prelude hiding (error)
import System.Exit (ExitCode)

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
