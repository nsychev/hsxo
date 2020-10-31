module Hsxo.EasySpec
  ( spec
  ) where

import Test.Hspec


spec :: Spec
spec = do
  describe "test" $ do
    it "should be eq" $ do
      (2 + 2) `shouldBe` (4 :: Int)
