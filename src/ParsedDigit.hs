module ParsedDigit (ParsedDigit(..), toChar, fromChar, toDigit) where

import Data.Char (digitToInt)
import SevenSegmentDigit

data ParsedDigit = Unparsable | ParsedInt Int deriving Eq

toChar :: ParsedDigit -> Char
toChar Unparsable = '?'
toChar (ParsedInt n) = head (show n)

fromChar :: Char -> ParsedDigit
fromChar '?' = Unparsable
fromChar n = ParsedInt (digitToInt n)

toDigit :: ParsedDigit -> SevenSegmentDigit
toDigit Unparsable = "         "
toDigit (ParsedInt n) = digitStrings !! n
