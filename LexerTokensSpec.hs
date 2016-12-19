module LexerTokensSpec where

import Test.Hspec
import LexerTokens

main :: IO ()
main = hspec $ do
  describe "position in text" $ do
    it "line number" $
      lineNumber (Cursor 1 2) `shouldBe` 1

    it "column number" $
      column (Cursor 1 2) `shouldBe` 2

  describe "move position" $ do
    it "no move on empty" $
      moveCursor (Cursor 1 2) "" `shouldBe` Cursor 1 2

    it "move left" $
      moveCursor (Cursor 1 2) "abc" `shouldBe` Cursor 1 5

    it "move left ignoring \\r" $
      moveCursor (Cursor 1 2) "a\rt" `shouldBe` Cursor 1 4

    it "move left and down" $
      moveCursor (Cursor 1 2) "abc\n" `shouldBe` Cursor 2 1

    it "move left down left" $
      moveCursor (Cursor 1 2) "abc\nabc" `shouldBe` Cursor 2 4
