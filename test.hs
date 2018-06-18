{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Void
import           System.Exit
import           System.IO
import qualified System.Process as Raw (createPipe)
import           System.Process.Typed

data Shell i o a where
  Command :: String -> [String] -> Shell ByteString ByteString ExitCode
  Pipe :: Shell i ByteString a -> Shell ByteString o a -> Shell i o a
  Sequence :: Shell i _o a -> Shell _i o a -> Shell i o a
  Redirect :: Shell i ByteString a -> FilePath -> Shell i Void a
  Background :: Shell i _o a -> Shell i Void ThreadId
  Substitution :: Shell Void ByteString ByteString -> (ByteString -> Shell i o a) -> Shell i o a

interpret :: StreamSpec 'STInput () -> StreamSpec 'STOutput r -> Shell i o a -> IO a
interpret input output =
  \case
    Command cmd args -> do
      runProcess (setCloseFds True (setStdin input (setStdout output (proc cmd args))))
    Sequence x y -> interpret input output x >> interpret input output y
    Redirect src fp -> do
      handle <- openFile fp WriteMode
      interpret input (useHandleClose handle) src
    Background src -> forkIO (void (interpret input output src))
    Substitution src f -> do
      v <- interpret input byteStringOutput src
      interpret input output (f v)
    Pipe from to -> do
      (reader, writer) <- Raw.createPipe
      fmap
        snd
        (concurrently
           (interpret input (useHandleOpen writer) from <* hClose writer)
           (interpret (useHandleClose reader) output to))

main :: IO ()
main = do
  void
    (interpret
       inherit
       inherit
       (Pipe
          (Sequence
             (Command "echo" ["Begin!"])
             (Command "tail" ["-f", "hello.txt"]))
          (Redirect
             (Pipe
                (Command "grep" ["[a-zA-Z]*", "-o", "--line-buffered"])
                (Command "cat" []))
             "out.txt")))
