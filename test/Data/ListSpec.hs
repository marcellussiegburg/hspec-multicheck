module Data.ListSpec where

import Prelude
import Test.Hspec.Multicheck

spec :: Spec
spec =
  describe "length" $
    it "is higher for longer lists" $ property $ \ xs ->
      not (null xs) ==> length xs > length (tail (xs :: [Int]))

