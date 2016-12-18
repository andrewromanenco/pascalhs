module LexerSpec where

import Test.Hspec
import Lexer

main :: IO ()
main = hspec $ do
  describe "stripExistingPrefix" $ do
    it "curs off existing prefix" $
      stripExistingPrefix "pre" "prefix" `shouldBe` "fix"
