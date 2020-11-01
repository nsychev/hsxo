module Main
  ( main
  ) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Hsxo.Constants as C
import qualified Hsxo.Client as App


-- Main function for client.
-- Usage: hsxo-client host [port]
main :: IO ()
main = getArgs >>= process


-- Processes command-line arguments.
process :: [String] -> IO ()
process [host, port] = App.runClient host port
process [host] = App.runClient host C.defaultPort
process _ = do
  hPutStrLn stderr "Usage: hsxo-client host [port]"
  exitFailure
