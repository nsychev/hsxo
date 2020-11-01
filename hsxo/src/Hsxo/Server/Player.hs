module Hsxo.Server.Player
  ( checkWinner
  , findSmartMove
  , cell
  ) where

import Data.Maybe (fromMaybe)
import Data.List (findIndex, find)
import Lens.Micro ((.~), (&), ix)

import Hsxo.Constants (strikeLength)

import Hsxo.Field (Field, Player (..))
import Hsxo.Util (enumerate)


-- Converts 2D-coordinates to index.
cell :: Int -> Int -> Int -> Int
cell sz x y = x * sz + y


-- Tries to find winning combination at the field.
checkWinner :: Int -> Field -> Player
checkWinner sz f = fromMaybe Nobody (find hasWon [ServerPlayer, ClientPlayer])
  where
    hasWon :: Player -> Bool
    hasWon p = any (all (== p)) (rows ++ cols ++ mainD ++ sideD)

    rows :: [[Player]]
    rows = [[ f !! cell sz x y | y <- [start..start + strikeLength - 1] ] | x <- [0..sz-1], start <- [0..sz - strikeLength]]

    cols :: [[Player]]
    cols = [[ f !! cell sz x y | x <- [start..start + strikeLength - 1] ] | y <- [0..sz-1], start <- [0..sz - strikeLength]]

    mainD :: [[Player]]
    mainD = [[ f !! cell sz (x + t) (y + t) | t <- [0..strikeLength - 1]] | x <- [0..sz - strikeLength], y <- [0..sz - strikeLength]]

    sideD :: [[Player]]
    sideD = [[ f !! cell sz (x - t) (y + t) | t <- [0..strikeLength - 1]] | x <- [strikeLength - 1..sz - 1], y <- [0..sz - strikeLength]]


-- Tries to find an optimal move by heuristics of the most naive player realization.
-- Follows two rules:
-- 1. If it's possible to win in one move, do that move.
-- 2. If opponent can win in one move, block that move by own one.
findSmartMove :: Int -> Field -> Maybe Int
findSmartMove sz f = findWinMove & findSaveMove
  where
    findWinMove :: Maybe Int
    findWinMove = findIndex isWinMove $ enumerate f

    isWinMove :: (Int, Player) -> Bool
    isWinMove (i, c) = c == Nobody && checkWinner sz (f & ix i .~ ServerPlayer) == ServerPlayer

    findSaveMove :: Maybe Int -> Maybe Int
    findSaveMove Nothing = findIndex isSaveMove $ enumerate f
    findSaveMove other = other

    isSaveMove :: (Int, Player) -> Bool
    isSaveMove (i, c) = c == Nobody && checkWinner sz (f & ix i .~ ClientPlayer) == ClientPlayer

