module PolicyNumbers.Validate (validate) where

import PolicyNumber
import ParsedDigit

import Flow
import Data.Char (digitToInt)

-- | Checks whether a policy number is valid. A valid policy number has 9 digits
-- | and a valid checksum, which is calculated as follows:
-- |
-- | policy number:   3  4  5  8  8  2  8  6  5
-- | position names: d9 d8 d7 d6 d5 d4 d3 d2 d1
-- | checksum calculation:
-- |   (d1+(2*d2)+(3*d3)+...+(9*d9)) mod 11 = 0
--
-- Examples:
--
-- >>> :{
--  let
--    inputs = map fromStr
--      [ "345882865"
--      , "457508000"
--      , "123456789"
--      , "000000000"
--      , "111111110"
--      , "322222222" ]
--  in all (== True) <| map validate inputs
-- :}
-- True
--
-- >>> :{
--  let
--    inputs = map fromStr
--      [ "987654321"
--      , "664371495"
--      , "333333333"
--      , "999999999"
--      , "12345678"
--      , "000" ]
--  in all (== False) <| map validate inputs
-- :}
-- True
validate :: PolicyNumber -> Bool
validate pn
  | Unparsable `elem` pn = False
  | length pn /= 9 = False
  | otherwise = (dotProduct `mod` 11) == 0
  where
    reverseIntegers = map digitToInt <| reverse <| toStr pn
    dotProduct = sum <| zipWith (*) [1..9] reverseIntegers
