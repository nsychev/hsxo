{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hsxo.Client.UI
  ( startApp
  ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as BC
import qualified Graphics.Vty as V

import Brick ((<+>), (<=>))
import Lens.Micro (Lens', (^?!), (^.), (^?), (%~), (&), _1, _2, ix, has)

import qualified Hsxo.Client.State as S
import qualified Hsxo.Message as M
import qualified Hsxo.ProtoHelpers as Proto

import Hsxo.Field (Player (..))
import Hsxo.Util (cell)
import Data.List (intersperse)
import Data.List.Split (chunksOf)


-- Style attributes for Brick.
styleCursor :: B.AttrName
styleCursor = B.attrName "styleCursor"

attributes :: B.AttrMap
attributes = B.attrMap V.defAttr [(styleCursor, B.bg V.brightBlack)]


-- Represents direction of cursor movement.
data CursorDirection = MoveUp | MoveDown | MoveLeft | MoveRight


-- Moves the cursor.
moveCursor :: CursorDirection -> S.ClientGameState -> S.ClientGameState
moveCursor direction game = game & (case direction of
  MoveUp    -> safeChange _1 (subtract 1)
  MoveDown  -> safeChange _1 (+ 1)
  MoveLeft  -> safeChange _2 (subtract 1)
  MoveRight -> safeChange _2 (+ 1)) (game ^. S.size)


-- Changes value so that it will not exceed range [0..m)
-- safeChange :: (Int -> Int) -> Int -> Int -> Int
safeChange :: Lens' (Int, Int) Int -> (Int -> Int) -> Int -> S.ClientGameState -> S.ClientGameState
safeChange comp func m = S.cursor . comp %~ overF
  where
    overF old = let new = func old in
      if | new >= m -> m - 1
         | new < 0 -> 0
         | otherwise -> new


-- Makes game move.
makeMove :: S.ClientGameState -> IO S.ClientGameState
makeMove game = do
  let position = game ^. S.cursor
  let sock = game ^. S.sock
  Proto.sendStruct sock (uncurry M.mkGameMove position)
  answer :: M.GameState <- Proto.receiveStruct sock
  return $ S.overState game answer


-- Checks player can do the move.
currentCellFree :: S.ClientGameState -> Bool
currentCellFree game = (game ^. S.field ^?! ix i) == Nobody
  where
    i = uncurry (cell (game ^. S.size)) (game ^. S.cursor)


-- Checks if game is finished.
finished :: S.ClientGameState -> Bool
finished game = S.result `has` game


-- Handles keyboard event.
handleEvent :: S.ClientGameState -> B.BrickEvent () e -> B.EventM () (B.Next S.ClientGameState)
handleEvent game (B.VtyEvent (V.EvKey key [])) | finished game = case key of
  V.KChar 'q' -> B.halt game
  _           -> B.continue game
handleEvent game (B.VtyEvent (V.EvKey key [])) = case key of
  V.KUp       -> B.continue $ moveCursor MoveUp game
  V.KDown     -> B.continue $ moveCursor MoveDown game
  V.KLeft     -> B.continue $ moveCursor MoveLeft game
  V.KRight    -> B.continue $ moveCursor MoveRight game
  V.KChar ' '
    | currentCellFree game
              -> B.suspendAndResume $ makeMove game
  V.KChar 'q' -> B.halt game
  _           -> B.continue game
handleEvent game _ = B.continue game


-- Converts Player to Brick Widget with X, O or empty string respectively.
fromPlayer :: Player -> B.Widget ()
fromPlayer p = case p of
  ServerPlayer -> B.str "X"
  ClientPlayer -> B.str "O"
  Nobody       -> B.str " "


-- Highlights current cell.
highlight :: S.ClientGameState -> [B.Widget ()] -> [B.Widget ()]
highlight game w = w & ix (uncurry (cell (game ^. S.size)) (game ^. S.cursor)) %~ B.withDefAttr styleCursor


-- Draws game field.
drawField :: S.ClientGameState -> B.Widget ()
drawField game =
  B.padRight (B.Pad 1) $
  BC.hCenter $
  B.withBorderStyle BS.unicode $
  BB.border $
  B.hLimit limit $
  B.vLimit limit $
  B.vBox $
  intersperse (B.withBorderStyle BS.unicode BB.hBorder) $
  B.hBox .
  intersperse (B.withBorderStyle BS.unicode BB.vBorder) <$>
  chunksOf (game ^. S.size)
  (highlight game $ map fromPlayer (game ^. S.field))
  where
    limit = game ^. S.size * 2 - 1

-- Draws help box.
drawHelp :: B.Widget ()
drawHelp =
  B.hLimit 20 $
  B.withBorderStyle BS.unicode $
  BB.borderWithLabel (B.str " Usage ") $
  B.padLeftRight 1 $
  B.str "select cell: ↓↑←→\nmove: enter\nquit: q"


-- Draws status bar.
drawStatus :: S.ClientGameState -> B.Widget ()
drawStatus game = case game ^? S.result of
  Nothing -> B.emptyWidget
  Just result -> B.vLimit 3 $ case result ^? S.cause of
    Just cause -> B.withBorderStyle BS.unicodeBold $
                  BB.border $
                  BC.center $
                  B.str cause
    Nothing    -> B.withBorderStyle BS.unicodeBold $
                  BB.border $
                  BC.center $
                  B.str $ "Game finished. " ++ case result ^. S.winner of
                    Nobody -> "Draw"
                    ServerPlayer -> "You lose"
                    ClientPlayer -> "You win"


-- Merges widgets.
drawGame :: S.ClientGameState -> B.Widget ()
drawGame game = (drawField game <+> drawHelp) <=> drawStatus game


-- The main application.
app :: B.App S.ClientGameState e ()
app = B.App
  { B.appDraw         = \x -> [drawGame x]
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent  = handleEvent
  , B.appStartEvent   = pure
  , B.appAttrMap      = const attributes
  }


-- Helper to start the app.
startApp :: S.ClientGameState -> IO S.ClientGameState
startApp = B.defaultMain app
