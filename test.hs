{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-- | A shell experiment.
module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.IO.Handle
import           Prelude hiding (error)
import           System.Exit
import           System.IO
import qualified System.Process as Raw (createPipe)
import           System.Process.Typed

--------------------------------------------------------------------------------
-- DSL

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

--------------------------------------------------------------------------------
-- Interpreter

-- | Interpret a shell.
interpret ::
     Handle -- ^ Input stream.
  -> Handle -- ^ Output stream.
  -> Handle -- ^ Err stream.
  -> Shell i o r -- ^ Shell to run.
  -> IO r -- ^ Output from the shell.
interpret input output error =
  \case
    Command cmd args -> do
      runProcess
        (setCloseFds
           True
           (setStderr
              (useHandleOpen error)
              (setStdin
                 (useHandleOpen input)
                 (setStdout
                    (useHandleOpen output)
                    (proc (T.unpack cmd) (map T.unpack args))))))
    Sequence x y -> do
      _ <- interpret input output error x
      interpret input output error y
    Redirect src redirect ->
      case redirect of
        StdoutTo to -> do
          (h, final) <- getTo to
          interpret input h error src `finally` final
        StderrTo to -> do
          (h, final) <- getTo to
          interpret input output h src `finally` final
      where getTo to =
              case to of
                ToStderr -> pure (error, pure ())
                ToStdout -> pure (output, pure ())
                ToFile fp -> do
                  h <- openFile fp WriteMode
                  pure (h, hClose h)
                ToFileAppend fp -> do
                  h <- openFile fp AppendMode
                  pure (h, hClose h)
    Background src -> forkIO (void (interpret input output error src))
    Substitution src f -> do
      (reader, writer) <- Raw.createPipe
      _ <- interpret input writer error src `finally` hClose writer
      o <- S.hGetContents reader
      interpret input output error (f o)
    Pipe from to -> do
      (reader, writer) <- Raw.createPipe
      (_, v) <-
        concurrently
          (interpret input writer error from `finally` hClose writer)
          (interpret reader output error to `finally` hClose reader)
      pure v
    Conduit c -> do
      hSetBuffering input NoBuffering
      liftIO (hSetBuffering output NoBuffering)
      runConduit (CB.sourceHandle input .| c .| CB.sinkHandle output)

--------------------------------------------------------------------------------
-- Examples

-- | Example use.
main :: IO ()
main = do
  void (interpret stdin stdout stderr tailDemo)

conduitDemo :: Shell ByteString ByteString ExitCode
conduitDemo =
  Pipe
    (Command "tail" ["-f", "nums.txt"])
    (Pipe (Conduit
             (CA.conduitParser (Atto.decimal <* Atto.endOfLine) .|
              CL.map snd .|
              CL.map ((* 2) :: Int -> Int) .|
              CL.map (S8.pack . (++ "\n") . show)))
          (Command "grep" ["[123]"]))

backgrounding :: Shell ByteString ByteString ExitCode
backgrounding =
  Sequence
    (Background (Sequence (Command "sleep" ["1"]) (Command "echo" ["done!"])))
    (Sequence (Command "echo" ["Doing..."]) (Command "sleep" ["2"]))

subbing :: Shell ByteString ByteString ExitCode
subbing =
  Substitution
    (Command "which" ["ls"])
    (\output -> Command "echo" [T.decodeUtf8 ("The output was: " <> output)])

sequencing :: Shell ByteString ByteString ExitCode
sequencing =
  Sequence
    (Redirect (Command "echo" ["hi"]) (StdoutTo (ToFile "x.txt")))
    (Command "echo" ["done"])

tailDemo :: Shell ByteString () ExitCode
tailDemo =
  Pipe
    (Redirect (Command "tail" ["-f", "hello.txt"])
              (StderrTo ToStdout))
    (Redirect
       (Pipe
          (Command "grep" ["[a-zA-Z]*", "-o", "--line-buffered"])
          (Command "cat" []))
       (StdoutTo (ToFile "out.txt")))
