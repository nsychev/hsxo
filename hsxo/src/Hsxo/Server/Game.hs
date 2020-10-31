{-# LANGUAGE ScopedTypeVariables #-}

module Hsxo.Server.Game
 ( playGame
 ) where

import qualified Data.ProtocolBuffers as PB
import qualified Network.Socket as NS

import Control.Exception.Base (catch, SomeException)
import System.IO (hPrint, stderr)

import qualified Hsxo.Message as Message
import qualified Hsxo.ProtoHelpers as Proto

-- Handles a connection in a thread. Plays the game.
playGame :: (NS.Socket, NS.SockAddr) -> IO ()

playGame (sock, addr) = do
  do
    putStrLn $ "Accepting connection from " ++ show addr

    let helloServer = Message.HelloServer {
      Message.version = PB.putField 1
    }
    _ <- Proto.sendStruct sock helloServer
    helloClient :: Message.HelloClient <- Proto.receiveStruct sock
    print helloClient
    print $ PB.getField $ Message.size helloClient
    NS.close sock
  `catch` (\e -> do
    hPrint stderr (e :: SomeException)
  )
