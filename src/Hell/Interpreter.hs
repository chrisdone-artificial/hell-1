{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

-- | Interpreter of the Hell language.

module Hell.Interpreter where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T
import           Hell.Types
import           Prelude hiding (error)
import           System.Directory
import           System.Exit
import           System.IO
import qualified System.Process as Raw (createPipe)
import           System.Process.Typed

--------------------------------------------------------------------------------
-- Interpreter

-- | Interpret some shell.
interpretSomeAction ::
     Handle -- ^ Input stream.
  -> Handle -- ^ Output stream.
  -> Handle -- ^ Err stream.
  -> SomeAction -- ^ Action to run.
  -> IO () -- ^ Output from the shell.
interpretSomeAction i o e (SomeAction s) = void (interpret i o e s)

-- | Interpret a shell.
interpret ::
     Handle -- ^ Input stream.
  -> Handle -- ^ Output stream.
  -> Handle -- ^ Err stream.
  -> Action r -- ^ Action to run.
  -> IO r -- ^ Output from the shell.
interpret input output error =
  \case
    Command cmd args -> do
      runProcess (config (proc (T.unpack cmd) (map T.unpack args)))
      where config =
              setDelegateCtlc True .
              setCloseFds True .
              setStderr (useHandleOpen error) .
              setStdin (useHandleOpen input) . setStdout (useHandleOpen output)
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
    ChangeDirectory fp ->
      case fp of
        Chdir dir ->
          catch
            (ExitSuccess <$ setCurrentDirectory dir)
            (\(_ :: IOException) -> pure (ExitFailure 1))
        GoHome -> do
          dir <- getHomeDirectory
          catch
            (ExitSuccess <$ setCurrentDirectory dir)
            (\(_ :: IOException) -> pure (ExitFailure 1))
