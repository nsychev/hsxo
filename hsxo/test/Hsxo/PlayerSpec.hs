module Hsxo.PlayerSpec
  ( spec
  ) where

import Test.Hspec

import Hsxo.Server.Player (checkWinner, findSmartMove)
import Hsxo.Field (Player (..))


spec :: Spec
spec = do
  describe "checkWinner" $ do
    it "Row" $ do
      ServerPlayer `shouldBe` checkWinner 3 [ServerPlayer, ServerPlayer, ServerPlayer,
                                             Nobody      , Nobody      , Nobody      ,
                                             Nobody      , Nobody      , Nobody      ]
    it "Column" $ do
      ClientPlayer `shouldBe` checkWinner 3 [ServerPlayer, ServerPlayer, ClientPlayer,
                                             Nobody      , ClientPlayer, ClientPlayer,
                                             ServerPlayer, Nobody      , ClientPlayer]

    it "Main diagonal" $ do
      ClientPlayer `shouldBe` checkWinner 4 [Nobody      , ClientPlayer, ServerPlayer, Nobody      ,
                                             Nobody      , Nobody      , ClientPlayer, ServerPlayer,
                                             ServerPlayer, Nobody      , Nobody      , ClientPlayer,
                                             Nobody      , Nobody      , ServerPlayer, Nobody      ]

    it "Side diagonal" $ do
      ServerPlayer `shouldBe` checkWinner 4 [Nobody      , ServerPlayer, ServerPlayer, ClientPlayer,
                                             ServerPlayer, ClientPlayer, ClientPlayer, ServerPlayer,
                                             Nobody      , ServerPlayer, ClientPlayer, ClientPlayer,
                                             Nobody      , Nobody      , ServerPlayer, Nobody      ]

    it "Nobody" $ do
      Nobody `shouldBe` checkWinner 3 [ServerPlayer, ClientPlayer, ServerPlayer,
                                       ServerPlayer, ClientPlayer, ClientPlayer,
                                       ClientPlayer, ServerPlayer, ClientPlayer]

  describe "findSmartMove" $ do
    it "Finds win move" $ do
      Just 0 `shouldBe` findSmartMove 3 [Nobody      , Nobody      , ClientPlayer,
                                         ServerPlayer, ServerPlayer, ClientPlayer,
                                         ServerPlayer, ClientPlayer, Nobody      ]

    it "Finds anti-loss move" $ do
      Just 8 `shouldBe` findSmartMove 3 [ClientPlayer, Nobody      , Nobody      ,
                                         Nobody      , ClientPlayer, Nobody      ,
                                         ServerPlayer, Nobody      , Nobody      ]

    it "No smart move available" $ do
      Nothing `shouldBe` findSmartMove 3 [ClientPlayer, Nobody      , Nobody      ,
                                          Nobody      , ServerPlayer, ClientPlayer,
                                          Nobody      , Nobody      , Nobody      ]
