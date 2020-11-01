module Main
  ( main
  ) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Hsxo.Constants as C
import qualified Hsxo.Server as App


-- Main function for server.
-- Usage: hsxo-server [port]
main :: IO ()
main = getArgs >>= process


-- Processes command-line arguments.
process :: [String] -> IO ()
process [port] = App.runServer port
process [] = App.runServer C.defaultPort
process _ = do
  hPutStrLn stderr "Usage: hsxo-server [port]"
  exitFailure
