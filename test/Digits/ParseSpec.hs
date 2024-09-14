module Digits.ParseSpec where

import Digits.Parse
import PolicyNumber
import Test.Hspec

input :: [[String]]
input =
  [ [ " _ \
      \| |\
      \|_|",
      " _ \
      \| |\
      \|_|"
    ],
    [ "   \
      \  |\
      \  |",
      " _ \
      \ _|\
      \|_ "
    ]
  ]

output :: [PolicyNumber]
output = [fromString "00", fromString "12"]

spec :: Spec
spec = do
  describe "parse" $ do
    it "transforms seven-segment digit strings into PolicyNumbers" $ do
      map parse input `shouldBe` output
