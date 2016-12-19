-- | Utils.

module Utils (stripExistingPrefix, (=~+), match_token) where

import Data.List
import Data.Maybe
import Text.Regex.PCRE


-- | Cuts off existing prefix from a string. Attempt to cut off non existing
-- prefix will fail at runtime.
stripExistingPrefix :: [Char] -> [Char] -> [Char]
stripExistingPrefix  prefix str = fromJust (stripPrefix prefix str)


-- | Case insensitive regex: http://stackoverflow.com/a/1008334
ciMatch ::
   ( RegexMaker regex compOpt execOpt source
   , RegexContext regex source1 target )
   => source1 -> (source, compOpt, execOpt) -> target
source1 `ciMatch` (source, compOpt, execOpt)
  = match (makeRegexOpts compOpt execOpt source) source1


-- | Case insensitive regex match. Return matched string or empty one.
(=~+) :: [Char] -> [Char] -> [Char]
str =~+ pattern = str `ciMatch` ("^" ++ pattern, compCaseless + compUTF8, execBlank) :: String


-- | Match regex to the beginning of given list. Case insensitive.
-- Result maybe is matched string and the rest of the list.
match_token :: [Char] -> [Char] -> Maybe([Char], [Char])
match_token pattern str = let matched = (str =~+ ("^" ++ pattern))
 in if matched == []
   then Nothing
   else Just (matched, stripExistingPrefix matched str)
