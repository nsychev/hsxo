{-# LANGUAGE ScopedTypeVariables #-}

module Hsxo.Client.Game
 ( playGame
 ) where

import qualified Network.Socket as NS

import Control.Monad (when, void)
import Lens.Micro ((^.))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import qualified Hsxo.Client.State as S
import qualified Hsxo.Client.UI as UI
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

  size <- readSafeSize
  Proto.sendStruct sock $ M.mkHelloClient size

  state :: M.GameState <- Proto.receiveStruct sock
  void $ UI.startApp $ S.mkClientGameState sock size state
  putStrLn "Thanks for the game!"


-- Safe read integer.
readSafeSize :: IO Int
readSafeSize = do
  putStrLn "Type the field size (single integer [3..10]):"
  sizeS :: String <- getLine
  case readMaybe sizeS :: Maybe Int of
    Nothing -> do
      putStrLn "It's not an integer."
      readSafeSize
    Just x | x < 3 || x > 10 -> do
      putStrLn "It's not in range."
      readSafeSize
    Just x -> return x
