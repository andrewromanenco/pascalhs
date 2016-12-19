module LexerSpec where

import Test.Hspec
import Lexer
import LexerTokens

main :: IO ()
main = hspec $ do
  describe "tokenize input to list" $ do
    it "empty" $
      tokenize "" `shouldBe` [EOF]

    it "expression" $
      tokenize "if x<y then a:=45" `shouldBe` [
        IF  (Cursor 1 1), WS (Cursor 1 3) " ",
        IDENT (Cursor 1 4) "x", LT_ (Cursor 1 5),IDENT (Cursor 1 6) "y",WS (Cursor 1 7) " ",
        THEN (Cursor 1 8), WS (Cursor 1 12) " ",
        IDENT (Cursor 1 13) "a", ASSIGN (Cursor 1 14), NUM_INT (Cursor 1 16) "45", EOF]

    it "multiline" $
      tokenize "45\n34a" `shouldBe` [
        NUM_INT (Cursor 1 1) "45",
        WS (Cursor 1 3) "\n",
        NUM_INT (Cursor 2 1) "34",
        IDENT (Cursor 2 3) "a",
        EOF]

  describe "tokenize samples" $ do
    it "subscripts.pas" $ do
      input <- readFile "res/examples/subscripts.pas"
      tokenize input `shouldEndWith` [EOF]

    it "set.pas" $ do
      input <- readFile "res/examples/set.pas"
      tokenize input `shouldEndWith` [EOF]

    it "pointer.pas" $ do
      input <- readFile "res/examples/pointer.pas"
      tokenize input `shouldEndWith` [EOF]

    it "passfail.pas" $ do
      input <- readFile "res/examples/passfail.pas"
      tokenize input `shouldEndWith` [EOF]

    it "nesting.pas" $ do
      input <- readFile "res/examples/nesting.pas"
      tokenize input `shouldEndWith` [EOF]

    it "linkedlist2.pas" $ do
      input <- readFile "res/examples/linkedlist2.pas"
      tokenize input `shouldEndWith` [EOF]

    it "if.pas" $ do
      input <- readFile "res/examples/if.pas"
      tokenize input `shouldEndWith` [EOF]

    it "helloworld.pas" $ do
      input <- readFile "res/examples/helloworld.pas"
      tokenize input `shouldEndWith` [EOF]

    it "fact.pas" $ do
      input <- readFile "res/examples/fact.pas"
      tokenize input `shouldEndWith` [EOF]

    it "case.pas" $ do
      input <- readFile "res/examples/case.pas"
      tokenize input `shouldEndWith` [EOF]

    it "bubble.pas" $ do
      input <- readFile "res/examples/bubble.pas"
      tokenize input `shouldEndWith` [EOF]

    it "array2.pas" $ do
      input <- readFile "res/examples/array2.pas"
      tokenize input `shouldEndWith` [EOF]

    it "array.pas" $ do
      input <- readFile "res/examples/array.pas"
      tokenize input `shouldEndWith` [EOF]

    it "add.pas" $ do
      input <- readFile "res/examples/add.pas"
      tokenize input `shouldEndWith` [EOF]
