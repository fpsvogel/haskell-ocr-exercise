module Digits.Parse (parseDigits) where

import SevenSegmentDigit
import PolicyNumber
import ParsedDigit

import Flow ()

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
--    output = [fromStr "00", fromStr "12"]
--  in map parseDigits input == output
-- :}
-- True
parseDigits :: [SevenSegmentDigit] -> PolicyNumber
parseDigits = map parseDigit

parseDigit :: String -> ParsedDigit
parseDigit
  " _ \
  \| |\
  \|_|" = ParsedInt 0

parseDigit
  "   \
  \  |\
  \  |" = ParsedInt 1

parseDigit
  " _ \
  \ _|\
  \|_ " = ParsedInt 2

parseDigit
  " _ \
  \ _|\
  \ _|" = ParsedInt 3

parseDigit
  "   \
  \|_|\
  \  |" = ParsedInt 4

parseDigit
  " _ \
  \|_ \
  \ _|" = ParsedInt 5

parseDigit
  " _ \
  \|_ \
  \|_|" = ParsedInt 6

parseDigit
  " _ \
  \  |\
  \  |" = ParsedInt 7

parseDigit
  " _ \
  \|_|\
  \|_|" = ParsedInt 8

parseDigit
  " _ \
  \|_|\
  \ _|" = ParsedInt 9

parseDigit _ = Unparsable
