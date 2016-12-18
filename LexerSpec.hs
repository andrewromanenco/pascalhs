module LexerSpec where

import Test.Hspec
import Lexer

main :: IO ()
main = hspec $ do
  describe "stripExistingPrefix" $ do
    it "curs off existing prefix" $
      stripExistingPrefix "pre" "prefix" `shouldBe` "fix"

  describe "match_token" $ do
    it "matches pattern to the front" $
      match_token "[0-9]+" "123abc" `shouldBe` Just ("123", "abc")

    it "does not matches if not in the front" $
      match_token "999" "x999abc" `shouldBe` Nothing

    it "match is case insensitive" $
      match_token "abc" "aBc123" `shouldBe` Just ("aBc", "123")

  describe "tokenize" $ do
    it "EOF" $
     tokenize "" `shouldBe` [EOF]

    it "AND" $
      tokenize "and" `shouldBe` [AND]

    it "ARRAY" $
     tokenize "array" `shouldBe` [ARRAY]

    it "WS" $
      tokenize "  \t  \r\n " `shouldBe` [WS "  \t  \r\n "]
