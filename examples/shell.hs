{-# LANGUAGE OverloadedStrings #-}

-- | Example use.

module Main where

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import           Data.Functor
import           Data.Monoid
import qualified Data.Text.Encoding as T
import           Hell
import           Hell.Lexer
import           System.Exit
import           System.IO

--------------------------------------------------------------------------------
-- Examples

-- | Example use.
main :: IO ()
main =
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
