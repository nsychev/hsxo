module Hsxo.Server
  ( runServer
  ) where

import qualified Control.Concurrent.Async as Async
import qualified Network.Socket as NS

import Control.Monad (forever)

import qualified Hsxo.Server.Game as Game


-- Runs the server at specified port.
runServer :: String -> IO ()
runServer port = do
  sock <- NS.socket NS.AF_INET NS.Stream 0
  NS.setSocketOption sock NS.ReuseAddr 1
  addr <- head <$> NS.getAddrInfo (Just (NS.defaultHints
                                          { NS.addrFlags = [NS.AI_PASSIVE]
                                          , NS.addrFamily = NS.AF_INET
                                          , NS.addrSocketType = NS.Stream
                                          }))
                                  Nothing
                                  (Just port)
  NS.bind sock (NS.addrAddress addr)
  NS.listen sock 100
  forever $ handleConnection sock


-- Handles a connection and passes it to an async thread.
handleConnection :: NS.Socket -> IO ()
handleConnection sock = do
  putStrLn "Try handling..."
  conn <- NS.accept sock
  _ <- Async.async (Game.playGame conn)
  return ()
