module Digits.ParseSpec where

import Test.Hspec
import Digits.Parse
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
  describe "parse" $ do
    it "transforms seven-segment digit strings into PolicyNumbers" $ do
      map parse input `shouldBe` output
