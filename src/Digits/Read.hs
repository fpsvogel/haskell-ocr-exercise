module Digits.Read (readDigits) where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import EntriesFile
import Flow
import SevenSegmentDigit

-- | Transforms policy file contents into lists of seven-segment digit strings.
--
-- Examples:
--
-- >>> :{
--  let
--    input =
--      " _  _ \n\
--      \| || |\n\
--      \|_||_|\n\
--      \      \n\
--      \    _ \n\
--      \  | _|\n\
--      \  ||_ \n\
--      \      "
--    output =
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
--  in readDigits input == output
-- :}
-- True
readDigits :: EntriesFile -> [[SevenSegmentDigit]]
readDigits = splitEntries .> transposeToDigits

splitEntries :: String -> [[String]]
splitEntries =
  lines
    .> chunksOf entryHeightInLines
    .> map (take (entryHeightInLines - entryTrailingEmptyLines))
  where
    entryHeightInLines = 4
    entryTrailingEmptyLines = 1

transposeToDigits :: [[String]] -> [[String]]
transposeToDigits =
  map <|
    map (chunksOf entryWidthInChars)
      .> transpose
      .> map concat
  where
    entryWidthInChars = 3
