module PolicyNumbers.Status (Status (..), status) where

import Flow
import ParsedDigit
import PolicyNumber
import PolicyNumbers.Corrections
import PolicyNumbers.Validate

data Status = Valid | Error | Illegible | Ambiguous deriving (Eq)

instance Show Status where
  show :: Status -> String
  show Valid = "OK"
  show Error = "ERR"
  show Illegible = "ILL"
  show Ambiguous = "AMB"

-- | Returns a PolicyNumber's Status.
--
-- Examples:
--
-- >>> :{
--  let
--    input = fromString "345882865"
--  in status input
-- :}
-- OK
--
-- >>> :{
--  let
--    input = fromString "987654321"
--  in status input
-- :}
-- ERR
--
-- >>> :{
--  let
--    input = fromString "??5882865"
--  in status input
-- :}
-- ILL
status :: PolicyNumber -> Status
status pn
  | Unparsable `elem` pn = Illegible
  | not <| validate pn = Error
  | otherwise = Valid
