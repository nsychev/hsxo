module Hsxo.Field
  ( Player (..)
  , Field
  ) where

-- Represents player or nobody (draw or empty cell).
data Player = ServerPlayer | ClientPlayer | Nobody deriving (Enum, Show, Eq)


-- Represents game field.
type Field = [Player]
