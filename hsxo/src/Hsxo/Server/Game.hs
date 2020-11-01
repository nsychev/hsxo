{-# LANGUAGE ScopedTypeVariables #-}

module Hsxo.Server.Game
 ( playGame
 ) where

import qualified Network.Socket as NS

import Control.Arrow ((>>>))
import Control.Exception.Base (catch, SomeException)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, runStateT, gets, modify)
import Lens.Micro ((.~), ix, (^.), (^?!))
import System.IO (hPutStrLn, stderr)
import System.Random (randomIO, randomRIO)

import qualified Hsxo.Message as M
import qualified Hsxo.ProtoHelpers as Proto
import qualified Hsxo.Server.State as S
import qualified Hsxo.Server.Player as P

import Hsxo.Field (Player (..), Field)
import Hsxo.Util (enumerate)


-- Sends “Game over” event.
sendGameOver :: NS.Socket -> Field -> Player -> Maybe String -> IO ()
sendGameOver s f p r = Proto.sendStruct s $ M.mkGameState f $ Just $ M.mkGameResult p r


-- Sends “Game over” event using game state.
sendGameOverS :: Player -> Maybe String -> S.GameStateT IO ()
sendGameOverS p r = do
  state <- get
  liftIO $ sendGameOver (state ^. S.sock) (state ^. S.field) p r


-- Finishes game if someone has won.
ensureWinner :: S.GameStateT IO () -> S.GameStateT IO ()
ensureWinner next = do
  state <- get
  case P.checkWinner (state ^. S.size) (state ^. S.field) of
    Nobody -> if Nobody `elem` (state ^. S.field)
      then next
      else sendGameOverS Nobody Nothing
    player -> sendGameOverS player Nothing


-- Modifies selected cell in the field.
markCell :: Player -> Int -> S.GameStateT IO ()
markCell p i = modify $ S.field . ix i .~ p


-- Gets random move.
getRandomNobody :: Field -> IO Int
getRandomNobody f = do
  let values = filter (\(_, v) -> v == Nobody) (enumerate f)
  item <- randomRIO (0, length values - 1)
  return $ fst $ values !! item


-- Finds a move.
getServerMove :: S.GameStateT IO Int
getServerMove = do
  size <- gets (^. S.size)
  field <- gets (^. S.field)
  case P.findSmartMove size field of
    Nothing -> liftIO $ getRandomNobody field
    Just i -> return i


-- Waits for client's move and verifies it, then continues the game.
clientMoveLoop :: S.GameStateT IO ()
clientMoveLoop = do
    state <- get
    liftIO $ Proto.sendStruct (state ^. S.sock) $ M.mkGameState (state ^. S.field) Nothing

    move :: M.GameMove <- liftIO $ Proto.receiveStruct (state ^. S.sock)

    let x = move ^. M.x
    let y = move ^. M.y

    validateMove x Nothing
      >>= validateMove y
      >>= markCellClient (P.cell (state ^. S.size) x y)
      >>= switchPlayer
  where
    validateMove :: Int -> Maybe String -> S.GameStateT IO (Maybe String)
    validateMove x Nothing = do
      size <- gets (^. S.size)
      if x < 0 || x >= size
        then return $ Just "Invalid cell"
        else return Nothing
    validateMove _ bad = pure bad

    markCellClient :: Int -> Maybe String -> S.GameStateT IO (Maybe String)
    markCellClient i Nothing = do
      cell <- gets ((^. S.field) >>> (^?! ix i))
      case cell of
        Nobody -> markCell ClientPlayer i >> return Nothing
        _      -> return $ Just "You can't move to occupied cell"
    markCellClient _ bad = pure bad

    switchPlayer :: Maybe String -> S.GameStateT IO ()
    switchPlayer Nothing = serverMoveLoop
    switchPlayer err     = sendGameOverS Nobody err


-- Makes a move and checks if somebody has won, then continues the game.
serverMoveLoop :: S.GameStateT IO ()
serverMoveLoop = ensureWinner (getServerMove >>= markCell ServerPlayer)
                  >> ensureWinner clientMoveLoop


-- Handles a connection and starts the game.
playGame :: (NS.Socket, NS.SockAddr) -> IO ()
playGame (sock, addr) = do
  do
    hPutStrLn stderr $ "Accepting connection from " ++ show addr

    let helloServer = M.mkHelloServer
    Proto.sendStruct sock helloServer

    helloClient :: M.HelloClient <- Proto.receiveStruct sock
    let size = helloClient ^. M.size

    if size >= 3 && size <= 10
      then do
        -- randomly choose first player
        clientFirst :: Bool <- randomIO

        void $ runStateT
          (if clientFirst
            then clientMoveLoop
            else serverMoveLoop)
          (S.ServerGameState
            sock
            (replicate (size * size) Nobody)
            size)
      else sendGameOver sock (replicate 9 Nobody) Nobody $ Just "Invalid field size."
  `catch` (\e -> do
    hPutStrLn stderr $ "Exception from " ++ show addr ++ ": " ++ show (e :: SomeException)
  )

