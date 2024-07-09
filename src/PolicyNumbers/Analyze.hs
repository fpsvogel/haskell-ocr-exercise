module PolicyNumbers.Analyze (analyze) where

import PolicyNumber
import PolicyNumbers.Status

import Flow

-- | Analyzes a PolicyNumber.
--
-- Examples:
--
-- >>> :{
--  let
--    input = fromStr "345882865"
--  in analyze input
-- :}
-- "345882865"
--
-- >>> :{
--  let
--    input = fromStr "987654321"
--  in analyze input
-- :}
-- "987654321\tERR"
--
-- >>> :{
--  let
--    input = fromStr "??5882865"
--  in analyze input
-- :}
-- "??5882865\tILL"
analyze :: PolicyNumber -> String
analyze pn
  | status pn == Valid  = show pn
  | otherwise           = show pn ++ "\t" ++ show (status pn)
