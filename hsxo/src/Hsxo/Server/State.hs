module Hsxo.Server.State
  ( ServerGameState (ServerGameState)
  , sock
  , field
  , size
  , GameStateT
  ) where

import qualified Network.Socket as NS

import Control.Monad.State (StateT)
import Lens.Micro (Lens', lens)

import Hsxo.Field (Field)

data ServerGameState = ServerGameState
  { _sock :: NS.Socket
  , _field :: Field
  , _size :: Int
  }

sock :: Lens' ServerGameState NS.Socket
sock = lens _sock (\st s -> st { _sock = s })

field :: Lens' ServerGameState Field
field = lens _field (\st f -> st { _field = f })

size :: Lens' ServerGameState Int
size = lens _size (\st s -> st { _size = s })

type GameStateT = StateT ServerGameState
