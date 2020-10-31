{-# LANGUAGE ScopedTypeVariables #-}

module Hsxo.Client.Game
 ( playGame
 ) where

import qualified Data.ProtocolBuffers as PB
import qualified Network.Socket as NS

import Control.Monad (when)
import System.Exit (exitFailure)

import qualified Hsxo.Message as Message
import qualified Hsxo.ProtoHelpers as Proto


-- Handles a connection in a thread. Plays the game.
playGame :: NS.Socket -> IO ()

playGame sock = do
  helloServer :: Message.HelloServer <- Proto.receiveStruct sock
  let version = PB.getField $ Message.version helloServer
  when (version /= 1) $ do
    putStrLn $ "Invalid server version " ++ show version ++ " (supporting only 1), exiting"
    exitFailure

  let helloClient = Message.HelloClient { Message.size = PB.putField 5 }
  _ <- Proto.sendStruct sock helloClient

  NS.close sock
