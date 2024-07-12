module PolicyNumbers.Corrections (corrections) where

import PolicyNumber
import PolicyNumbers.Validate
import Digits.Corrections
import SevenSegmentDigit
import Digits.Parse
import ParsedDigit

import Flow

-- | Returns all possible corrections of the policy number specified by the given digit strings.
--
-- Examples:
--
-- >>> :{
--  let
--    input = toDigits (fromString "345082865")
--    output = map fromString ["345882865","345082885","345082869"]
--  in corrections input == output
-- :}
-- True
corrections :: [SevenSegmentDigit] -> [PolicyNumber]
corrections digits = map parse allCorrected |> filter validate
  where
    allCorrected = concatMap allCorrectedAt (filter (not <. null <. snd) <| zip [0..] digitsCorrections)
    digitsCorrections = map possibleCorrections digits
    allCorrectedAt (index, digitCorrections) = map (correctedAt index) digitCorrections
    correctedAt index digitCorrection = take index digits ++ [digitCorrection] ++ drop (index + 1) digits
