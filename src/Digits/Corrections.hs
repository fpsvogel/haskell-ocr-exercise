module Digits.Corrections (possibleCorrections) where

import Data.List (zipWith)
import Data.Maybe (mapMaybe)
import Flow
import SevenSegmentDigit

-- | Returns all possible corrections of a digit by adding a pipe or underscore.
--
-- Examples:
--
-- >>> :{
--  let
--    input =
--      " _ \
--      \| |\
--      \|_|"
--    output =
--      [
--        " _ \
--        \|_|\
--        \|_|"
--      ]
--  in possibleCorrections input == output
-- :}
-- True
--
-- >>> :{
--  let
--    input =
--      " _ \
--      \ _ \
--      \ _|"
--    output =
--      [
--        " _ \
--        \ _|\
--        \ _|"
--      , " _ \
--        \|_ \
--        \ _|"
--      ]
--  in possibleCorrections input == output
-- :}
-- True
possibleCorrections :: SevenSegmentDigit -> [SevenSegmentDigit]
possibleCorrections digitString =
  mapMaybe correct digitStrings
  where
    correct exemplar =
      let differences = filter (uncurry (/=)) (zip digitString exemplar)
       in if length differences == 1
            then
              let (myChar, exemplarChar) = head differences
               in if canBeCorrection myChar exemplarChar
                    then Just exemplar
                    else Nothing
            else Nothing

-- Checks if a character (first arg) can be a corrected to another (second arg),
-- i.e. the OCR machine may omit a pipe or underscore, but will never add an
-- extraneous pipe/underscore or confuse one with the other.
canBeCorrection :: Char -> Char -> Bool
canBeCorrection ' ' c = c == '|' || c == '_'
canBeCorrection _ _ = False