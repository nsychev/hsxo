{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Hsxo.Message where

import qualified Data.ProtocolBuffers as PB

import Data.Int (Int32)
import GHC.Generics (Generic)


newtype HelloServer = HelloServer
  { version :: PB.Required 1001 (PB.Value Int32)
  } deriving (Generic, Show)

instance PB.Encode HelloServer
instance PB.Decode HelloServer


newtype HelloClient = HelloClient
  { size :: PB.Required 1011 (PB.Value Int32)
  } deriving (Generic, Show)

instance PB.Encode HelloClient
instance PB.Decode HelloClient


newtype GameStart = GameStart
  { playerFirst :: PB.Required 1021 (PB.Value Bool)
  } deriving (Generic, Show)

instance PB.Encode GameStart
instance PB.Decode GameStart


data GameResult = GameResult
  { playerWin :: PB.Required 1031 (PB.Value Bool)
  , cause :: PB.Optional 1032 (PB.Value String)
  } deriving (Generic, Show)

instance PB.Encode GameResult
instance PB.Decode GameResult


data CellState = Empty | MarkedX | MarkedO deriving (Enum, Show)


data GameState = GameState
  { board :: PB.Repeated 1041 (PB.Enumeration CellState)
  , state :: PB.Optional 1042 (PB.Message GameResult)
  } deriving (Generic, Show)

instance PB.Encode GameState
instance PB.Decode GameState


data GameMove = GameMove
  { x :: PB.Required 1051 (PB.Value Int32)
  , y :: PB.Required 1052 (PB.Value Int32)
  } deriving (Generic, Show)

instance PB.Encode GameMove
instance PB.Decode GameMove
