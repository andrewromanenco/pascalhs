-- | This module takes input string and splits it into list of tokens.

{-# LANGUAGE ViewPatterns #-}

module Lexer where

import Data.List
import Data.Maybe


-- | Cuts off existing prefix from a string. Attempt to cut off non existing
-- prefix will fail at runtime.
stripExistingPrefix :: [Char] -> [Char] -> [Char]
stripExistingPrefix  prefix str = fromJust (stripPrefix prefix str)
