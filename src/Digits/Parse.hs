module Digits.Parse (parseDigits) where

import SevenSegmentDigit
import PolicyNumber

import Flow

-- | Transforms seven-segment digit strings into number strings.
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
--    output = ["00","12"]
--  in map parseDigits input == output
-- :}
-- True
parseDigits :: [SevenSegmentDigit] -> PolicyNumber
parseDigits = map (parseDigit .> show) .> concat

parseDigit :: String -> Int
parseDigit
  " _ \
  \| |\
  \|_|" = 0

parseDigit
  "   \
  \  |\
  \  |" = 1

parseDigit
  " _ \
  \ _|\
  \|_ " = 2

parseDigit
  " _ \
  \ _|\
  \ _|" = 3

parseDigit
  "   \
  \|_|\
  \  |" = 4

parseDigit
  " _ \
  \|_ \
  \ _|" = 5

parseDigit
  " _ \
  \|_ \
  \|_|" = 6

parseDigit
  " _ \
  \  |\
  \  |" = 7

parseDigit
  " _ \
  \|_|\
  \|_|" = 8

parseDigit
  " _ \
  \|_|\
  \ _|" = 9

parseDigit _ = 0
