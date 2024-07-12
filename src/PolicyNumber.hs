module PolicyNumber (PolicyNumber, show, fromString, toDigits) where

import ParsedDigit
import SevenSegmentDigit

import Flow

type PolicyNumber = [ParsedDigit]

instance {-# OVERLAPPING #-} Show PolicyNumber where
  show :: PolicyNumber -> String
  show = map toChar

fromString :: String -> PolicyNumber
fromString = map fromChar

toDigits :: PolicyNumber -> [SevenSegmentDigit]
toDigits = map toDigit
