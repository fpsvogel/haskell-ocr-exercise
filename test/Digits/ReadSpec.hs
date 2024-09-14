module Digits.ReadSpec where

import Digits.Read
import Test.Hspec

input :: String
input =
  " _  _ \n\
  \| || |\n\
  \|_||_|\n\
  \      \n\
  \    _ \n\
  \  | _|\n\
  \  ||_ "

output :: [[String]]
output =
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

spec :: Spec
spec = do
  describe "readDigits" $ do
    it "transforms policy file contents into seven-segment digit strings" $ do
      readDigits input `shouldBe` output
