module Digits.ParseSpec where

import Test.Hspec
import Digits.Parse (parseDigits)

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

output :: [String]
output = ["00", "12"]

spec :: Spec
spec = do
  describe "parseDigits" $ do
    it "transforms seven-segment digit strings into number strings" $ do
      map parseDigits input `shouldBe` output
