module Digits.Parse (parse, digitStrings) where

import SevenSegmentDigit
import PolicyNumber
import ParsedDigit

import Flow
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- | Transforms seven-segment digit strings into PolicyNumbers.
--
-- Examples:
--
-- >>> :{
--  let
--    input =
--      [
--        [
--          " _ \
--          \| |\
--          \|_|"
--        , " _ \
--          \| |\
--          \|_|"
--        ],
--        [
--          "   \
--          \  |\
--          \  |"
--        , " _ \
--          \ _|\
--          \|_ "
--        ]
--      ]
--    output = [fromString "00", fromString "12"]
--  in map parse input == output
-- :}
-- True
parse :: [SevenSegmentDigit] -> PolicyNumber
parse = map parseDigit

parseDigit :: String -> ParsedDigit
parseDigit str = maybe Unparsable ParsedInt (elemIndex str digitStrings)
