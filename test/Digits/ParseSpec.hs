module Digits.ParseSpec where

import Test.Hspec
import Digits.Parse (parseDigits)
import PolicyNumber

input :: [[String]]
input =
  [
    [
      " _ \
      \| |\
      \|_|"
    , " _ \
      \| |\
      \|_|"
    ],
    [
      "   \
      \  |\
      \  |"
    , " _ \
      \ _|\
      \|_ "
    ]
  ]

output :: [PolicyNumber]
output = [fromStr "00", fromStr "12"]

spec :: Spec
spec = do
  describe "parseDigits" $ do
    it "transforms seven-segment digit strings into PolicyNumbers" $ do
      map parseDigits input `shouldBe` output
