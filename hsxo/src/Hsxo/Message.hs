{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Hsxo.Message
  ( HelloServer
  , mkHelloServer
  , version

  , HelloClient
  , mkHelloClient
  , size

  , GameResult
  , mkGameResult
  , winner
  , cause

  , GameState
  , mkGameState
  , field
  , result

  , GameMove
  , mkGameMove
  , x
  , y
  ) where

import qualified Data.ProtocolBuffers as PB

import Data.Int (Int32)
import GHC.Generics (Generic)
import Lens.Micro
  ( Lens', lens
  , Traversal'
  )

import qualified Hsxo.Constants as C

import Hsxo.Field (Field, Player)
import Hsxo.Util (intLens, maybeTraversal)


-- Welcome message from the server.
-- Contains protocol version to avoid incompatibility issues.
newtype HelloServer = HelloServer
  { _version :: PB.Required 11 (PB.Value Int32)
  } deriving (Generic, Show)

instance PB.Encode HelloServer
instance PB.Decode HelloServer

mkHelloServer :: HelloServer
mkHelloServer = HelloServer {
  _version = PB.putField $ fromIntegral C.version
}

version :: Lens' HelloServer Int
version = lens _version (\s v -> s { _version = v }) . PB.field . intLens


-- Welcome message from the client.
-- Contains field size by one dimension (results in field size × size).
newtype HelloClient = HelloClient
  { _size :: PB.Required 21 (PB.Value Int32)
  } deriving (Generic, Show)

instance PB.Encode HelloClient
instance PB.Decode HelloClient

mkHelloClient :: Int -> HelloClient
mkHelloClient sz = HelloClient {
  _size = PB.putField $ fromIntegral sz
}

size :: Lens' HelloClient Int
size = lens _size (\s sz -> s { _size = sz }) . PB.field . intLens


-- Server-originated message, transmitted as optional part of GameState.
-- Indicates game finish and always followed by closing connection.
data GameResult = GameResult
  { _winner :: PB.Required 41 (PB.Enumeration Player)
  , _cause :: PB.Optional 42 (PB.Value String)
  } deriving (Generic, Show)

instance PB.Encode GameResult
instance PB.Decode GameResult

mkGameResult :: Player -> Maybe String -> GameResult
mkGameResult p c = GameResult
  { _winner = PB.putField p
  , _cause = PB.putField c
  }

winner :: Lens' GameResult Player
winner = lens _winner (\s w -> s { _winner = w }) . PB.field

cause :: Traversal' GameResult String
cause = lens _cause (\s c -> s { _cause = c }) . PB.field . maybeTraversal


-- Server-originated message, containing current field state and
-- game result in case game is finished.
data GameState = GameState
  { _field :: PB.Repeated 51 (PB.Enumeration Player)
  , _result :: PB.Optional 52 (PB.Message GameResult)
  } deriving (Generic, Show)

instance PB.Encode GameState
instance PB.Decode GameState

mkGameState :: Field -> Maybe GameResult -> GameState
mkGameState f r = GameState
  { _field = PB.putField f
  , _result = PB.putField r
  }

field :: Lens' GameState [Player]
field = lens _field (\s f -> s { _field = f }) . PB.field

result :: Traversal' GameState GameResult
result = lens _result (\s r -> s { _result = r }) . PB.field . maybeTraversal


-- Client-originated message, containing its move.
data GameMove = GameMove
  { _moveX :: PB.Required 61 (PB.Value Int32)
  , _moveY :: PB.Required 62 (PB.Value Int32)
  } deriving (Generic, Show)

instance PB.Encode GameMove
instance PB.Decode GameMove

mkGameMove :: Int -> Int -> GameMove
mkGameMove mx my = GameMove
  { _moveX = PB.putField $ fromIntegral mx
  , _moveY = PB.putField $ fromIntegral my
  }

x :: Lens' GameMove Int
x = lens _moveX (\s p -> s { _moveX = p }) . PB.field . intLens

y :: Lens' GameMove Int
y = lens _moveY (\s p -> s { _moveY = p }) . PB.field . intLens
