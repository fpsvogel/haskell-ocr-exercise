module ParsedDigit (ParsedDigit(..), toChar, fromChar) where

import Data.Char (digitToInt)

data ParsedDigit = Unparsable | ParsedInt Int deriving Eq

toChar :: ParsedDigit -> Char
toChar Unparsable = '?'
toChar (ParsedInt n) = head (show n)

fromChar :: Char -> ParsedDigit
fromChar '?' = Unparsable
fromChar n = ParsedInt (digitToInt n)
