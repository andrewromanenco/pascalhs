module UtilsSpec where

import Test.Hspec
import Utils

main :: IO ()
main = hspec $ do
  describe "stripExistingPrefix" $ do
    it "curs off existing prefix" $
      stripExistingPrefix "pre" "prefix" `shouldBe` "fix"

  describe "case insensitive regex" $ do
    it "case insensitive match" $
      "aBc123" =~+ "ABC" `shouldBe` "aBc"

  describe "match_token to the beginning of input" $ do
    it "matches pattern to the front" $
      match_token "[0-9]+" "123abc" `shouldBe` Just ("123", "abc")

    it "does not matches if not in the front" $
      match_token "999" "x999abc" `shouldBe` Nothing

    it "match is case insensitive" $
      match_token "abc" "aBc123" `shouldBe` Just ("aBc", "123")
