module SevenSegmentDigit (SevenSegmentDigit, digitStrings) where

type SevenSegmentDigit = String

digitStrings :: [String]
digitStrings =
  [ " _ \
    \| |\
    \|_|", -- 0
    "   \
    \  |\
    \  |", -- 1
    " _ \
    \ _|\
    \|_ ", -- 2
    " _ \
    \ _|\
    \ _|", -- 3
    "   \
    \|_|\
    \  |", -- 4
    " _ \
    \|_ \
    \ _|", -- 5
    " _ \
    \|_ \
    \|_|", -- 6
    " _ \
    \  |\
    \  |", -- 7
    " _ \
    \|_|\
    \|_|", -- 8
    " _ \
    \|_|\
    \ _|" -- 9
  ]
