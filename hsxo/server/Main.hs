module Main
  ( main
  ) where

import Hsxo.Server


-- Just starts the endless listen loop.
main :: IO ()
main = runServer "4242"
