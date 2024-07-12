module PolicyNumbers.Analyze (analyze, analyzeAndCorrect) where

import PolicyNumber
import PolicyNumbers.Status
import PolicyNumbers.Corrections
import SevenSegmentDigit
import Digits.Parse

import Flow

-- | Returns a string with a policy number and (if it's invalid) its status.
--
-- Examples:
--
-- >>> :{
--  let
--    input = fromString "345882865"
--  in analyze input
-- :}
-- "345882865"
--
-- >>> :{
--  let
--    input = fromString "987654321"
--  in analyze input
-- :}
-- "987654321\tERR"
--
-- >>> :{
--  let
--    input = fromString "??5882865"
--  in analyze input
-- :}
-- "??5882865\tILL"
analyze :: PolicyNumber -> String
analyze pn
  | status pn == Valid  = show pn
  | otherwise           = showPolicyNumberAndStatus pn (status pn)

-- | Returns a string with a policy number (corrected, if possible) and (if it's
-- | invalid) its status.
--
-- Examples:
--
-- >>> :{
--  let
--    input = toDigits (fromString "345882865")
--  in analyzeAndCorrect input
-- :}
-- "345882865"
--
-- >>> :{
--  let
--    input = toDigits (fromString "123456189")
--  in analyzeAndCorrect input
-- :}
-- "123456789"
--
-- >>> :{
--  let
--    input = toDigits (fromString "000")
--  in analyzeAndCorrect input
-- :}
-- "000\tERR"
--
-- >>> :{
--  let
--    input = toDigits (fromString "??5882865")
--  in analyzeAndCorrect input
-- :}
-- "??5882865\tILL"
--
-- >>> :{
--  let
--    input = toDigits (fromString "345082865")
--  in analyzeAndCorrect input
-- :}
-- "345082865\tAMB"
analyzeAndCorrect :: [SevenSegmentDigit] -> String
analyzeAndCorrect digits
  | status (parse digits) == Valid  = show (parse digits)
  | otherwise =
    let correctedPn = corrections digits
        originalPn = parse digits
    in case length correctedPn of
      0 -> showPolicyNumberAndStatus originalPn (status originalPn)
      1 -> show (head correctedPn)
      _ -> showPolicyNumberAndStatus originalPn Ambiguous

showPolicyNumberAndStatus :: PolicyNumber -> Status -> String
showPolicyNumberAndStatus pn pnStatus = show pn ++ "\t" ++ show pnStatus
