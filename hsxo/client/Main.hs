module Main
  ( main
  ) where

import Hsxo.Client

main :: IO ()
main = runClient "127.0.0.1" "4242"
