module ParserSpec where

import Test.Hspec
import AST
import LexerTokens
import Parser


main :: IO ()
c = Cursor 3 4
main = hspec $ do
  describe "util functions" $ do
    it "filter out comments and ws" $ do
      filterOut [COMMA c, WS c "", SEMI c, COMMENT_1 c "", COMMENT_2 c "", EOF] `shouldBe`
        [COMMA c, SEMI c, EOF]

    it "maybeToken with match" $ do
      maybeToken "COMMA" [COMMA c, EOF] `shouldBe` (True, [EOF])

    it "maybeToken no match" $ do
      maybeToken "COMMA" [EOF] `shouldBe` (False, [EOF])

    it "mustBe good" $ do
      mustBe "Msg" [COMMA c, EOF] (\x -> Just ("COMMA", [EOF])) `shouldBe` ("COMMA", [EOF])

    -- TODO
    -- it "mustBe bad" $ do
    --   mustBe "Msg" [COMMA c, EOF] (\x -> Nothing) `shouldBe` ("COMMA", [EOF])

  describe "parse elements" $ do
    it "identifier" $ do
      parseIdentifier [IDENT c "name", EOF] `shouldBe` Just("name", [EOF])

    it "IdentifierList single" $ do
      parseIdentifierList [IDENT c "name", EOF] `shouldBe` (["name"], [EOF])

    it "IdentifierList multiple" $ do
      parseIdentifierList [IDENT c "name1", COMMA c, IDENT c "name2", COMMA c, IDENT c "name3", EOF]
        `shouldBe` (["name1", "name2", "name3"], [EOF])

    it "program heading: program" $
      parseProgramHeading [PROGRAM c, IDENT c "name", SEMI c]
        `shouldBe` Just (ProgramHeading "name" Nothing, [])

    it "program heading: program with id list" $
      parseProgramHeading [PROGRAM c, IDENT c "name", LPAREN c, IDENT c "var", RPAREN c, SEMI c]
        `shouldBe` Just (ProgramHeading "name" (Just ["var"]), [])

    it "program heading: unit" $
      parseProgramHeading [UNIT c, IDENT c "name", SEMI c]
        `shouldBe` Just (UnitHeading "name", [])
