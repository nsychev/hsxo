module Hsxo.Client
  ( runClient
  ) where

import qualified Network.Socket as NS
import Control.Exception (bracket)

import qualified Hsxo.Client.Game as Game


-- Starts the game using the server at host:port.
runClient :: String -> String -> IO ()
runClient host port = do
  addr <- head <$> NS.getAddrInfo (Just (NS.defaultHints { NS.addrSocketType = NS.Stream }))
                                    (Just host)
                                    (Just port)
  bracket (do
            sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
            NS.connect sock $ NS.addrAddress addr
            return sock)
          NS.close
          Game.playGame
