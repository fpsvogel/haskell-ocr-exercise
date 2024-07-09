module PolicyNumbers.Validate (validate) where

import PolicyNumber

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
--    inputs =
--      [ fromStr "345882865"
--      , fromStr "457508000"
--      , fromStr "123456789"
--      , fromStr "000000000"
--      , fromStr "111111110"
--      , fromStr "322222222" ]
--  in all (== True) <| map validate inputs
-- :}
-- True
--
-- >>> :{
--  let
--    inputs =
--      [ fromStr "987654321"
--      , fromStr "664371495"
--      , fromStr "333333333"
--      , fromStr "999999999"
--      , fromStr "12345678"
--      , fromStr "000" ]
--  in all (== False) <| map validate inputs
-- :}
-- True
validate :: PolicyNumber -> Bool
validate pn
  | length pn /= 9 = False
  | otherwise = (dotProduct `mod` 11) == 0
  where
    reverseIntegers = map digitToInt <| reverse <| toStr pn
    dotProduct = sum <| zipWith (*) [1..9] reverseIntegers
