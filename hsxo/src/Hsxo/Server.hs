module Hsxo.Server
  ( runServer
  ) where

-- import qualified Control.Concurrent.Async as Async
import qualified Network.Socket as NS

import Control.Monad (forever, void)

import qualified Hsxo.Server.Game as Game
import Control.Concurrent (forkFinally)


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
  conn <- NS.accept sock
  void $ forkFinally (Game.playGame conn) (\_ -> NS.close (fst conn))
