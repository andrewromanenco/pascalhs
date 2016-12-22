module ParserSpec where

import Test.Hspec
import Parser
import LexerTokens

main :: IO ()
c = Cursor 3 4
main = hspec $ do
  describe "parse elements" $ do
    it "program heading: unit" $
      parseProgramHeading [UNIT c, IDENT c "name", SEMI c] `shouldBe` Just (UnitHeading "name", [])

    it "identifier" $ do
      parseIdentifier [IDENT c "name", EOF] `shouldBe` Just("name", [EOF])

    it "identifier not found" $ do
      parseIdentifier [EOF] `shouldBe` Nothing
