{-# LANGUAGE ScopedTypeVariables #-}

module Hsxo.Client.Game
 ( playGame
 ) where

import qualified Network.Socket as NS

import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Lens.Micro

import qualified Hsxo.Constants as C
import qualified Hsxo.Message as M
import qualified Hsxo.ProtoHelpers as Proto


-- Handles a connection in a thread. Plays the game.
playGame :: NS.Socket -> IO ()

playGame sock = do
  helloServer :: M.HelloServer <- Proto.receiveStruct sock

  when ((helloServer ^. M.version) /= C.version) $ do
    hPutStrLn stderr $ "Invalid server version" ++
      show (helloServer ^. M.version) ++
      ", expected " ++
      show C.version
    exitFailure

  size :: Int <- readLn
  Proto.sendStruct sock $ M.mkHelloClient size

  playGameRec sock


playGameRec :: NS.Socket -> IO ()
playGameRec sock = do
  state :: M.GameState <- Proto.receiveStruct sock

  print $ state ^. M.field

  case state ^? M.result of
    Nothing -> do
      x <- readLn
      y <- readLn
      Proto.sendStruct sock $ M.mkGameMove x y
      playGameRec sock
    Just result -> do
      print result
