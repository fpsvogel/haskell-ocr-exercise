module PolicyNumber (PolicyNumber, show, fromString, toDigits) where

import Flow
import ParsedDigit
import SevenSegmentDigit

type PolicyNumber = [ParsedDigit]

instance {-# OVERLAPPING #-} Show PolicyNumber where
  show :: PolicyNumber -> String
  show = map toChar

fromString :: String -> PolicyNumber
fromString = map fromChar

toDigits :: PolicyNumber -> [SevenSegmentDigit]
toDigits = map toDigit
