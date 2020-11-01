module Hsxo.Client.State
  ( ClientGameResult
  , mkClientGameResult
  , winner
  , cause
  , ClientGameState
  , mkClientGameState
  , overState
  , sock
  , size
  , field
  , result
  , cursor
  ) where

import Network.Socket as NS

import Lens.Micro (Lens', Traversal', lens, (^.), (^?))

import Hsxo.Field (Field, Player (..))
import qualified Hsxo.Message as M
import Hsxo.Util (maybeTraversal)


-- Represents a game result without protobuf wrappers.
data ClientGameResult = ClientGameResult
  { _winner :: Player
  , _cause :: Maybe String
  }

winner :: Lens' ClientGameResult Player
winner = lens _winner (\s w -> s { _winner = w })

cause :: Traversal' ClientGameResult String
cause = lens _cause (\s c -> s { _cause = c }) . maybeTraversal


-- Helper to create ClientGameResult from server response.
mkClientGameResult :: M.GameResult -> ClientGameResult
mkClientGameResult r = ClientGameResult
  { _winner = r ^. M.winner
  , _cause  = r ^? M.cause
  }


-- Represents a game state on client side.
data ClientGameState = ClientGameState
  { _sock :: NS.Socket
  , _size :: Int
  , _field :: Field
  , _result :: Maybe ClientGameResult
  , _cursor :: (Int, Int)
  }

mkClientGameState :: NS.Socket -> Int -> M.GameState -> ClientGameState
mkClientGameState s sz state = ClientGameState
  { _sock = s
  , _size = sz
  , _field = state ^. M.field
  , _result = mkClientGameResult <$> state ^? M.result
  , _cursor = (0, 0)
  }

-- Modified ClientGameState using server response
overState :: ClientGameState -> M.GameState -> ClientGameState
overState game state = game
  { _field = state ^. M.field
  , _result = mkClientGameResult <$> state ^? M.result
  }

sock :: Lens' ClientGameState NS.Socket
sock = lens _sock (\s so -> s { _sock = so })

size :: Lens' ClientGameState Int
size = lens _size (\s sz -> s { _size = sz })

field :: Lens' ClientGameState Field
field = lens _field (\s f -> s { _field = f })

result :: Traversal' ClientGameState ClientGameResult
result = lens _result (\s r -> s { _result = r }) . maybeTraversal

cursor :: Lens' ClientGameState (Int, Int)
cursor = lens _cursor (\s c -> s { _cursor = c })
