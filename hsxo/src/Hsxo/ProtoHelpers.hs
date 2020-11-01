{-# LANGUAGE ScopedTypeVariables #-}

module Hsxo.ProtoHelpers
  ( HsxoProtoException (..)
  , receiveStruct
  , sendStruct
  ) where

import qualified Data.ProtocolBuffers as PB
import qualified Data.Serialize as DS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Control.Exception.Base
  ( throwIO
  , Exception
  )
import Control.Monad (void)

-- Exception class.
newtype HsxoProtoException = HsxoProtoDecodeException String
    deriving Show
instance Exception HsxoProtoException


-- Sends a protobuf-encodable structure to a socket.
sendStruct :: PB.Encode a => NS.Socket -> a -> IO ()
sendStruct sock value = do
  void $ NSB.send sock $ DS.runPut $ PB.encodeLengthPrefixedMessage value


-- Receives a protobuf-decodable structure from a socket.
receiveStruct :: PB.Decode a => NS.Socket -> IO a
receiveStruct sock = do
    firstByte <- NSB.recv sock 1
    processInput $ DS.runGetPartial PB.decodeLengthPrefixedMessage firstByte
  where
    processInput :: DS.Result a -> IO a
    processInput (DS.Fail message _) = throwIO $ HsxoProtoDecodeException message
    processInput (DS.Partial next) = do
      nextByte <- NSB.recv sock 1
      processInput $ next nextByte
    processInput (DS.Done r _) = return r
