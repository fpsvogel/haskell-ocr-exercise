module PolicyNumber (PolicyNumber, toStr, fromStr) where

import ParsedDigit

import Flow

type PolicyNumber = [ParsedDigit]

-- TODO find how to get around error "overlapping instances of Show"
instance {-# OVERLAPPING #-} Show PolicyNumber where
  show :: PolicyNumber -> String
  show = map toChar

toStr :: PolicyNumber -> String
toStr = map toChar

fromStr :: String -> PolicyNumber
fromStr = map fromChar
