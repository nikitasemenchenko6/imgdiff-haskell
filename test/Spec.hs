{-# LANGUAGE TypeInType #-}

import Format
import Lib
import Test.Hspec
import Test.QuickCheck
import RIO

main :: IO ()
main = hspec $ do
  describe "Lib.distance" $ do
    let orig       = "test-png-original.png"
    let scaledDown = "test-png-scaled-down.png"
    let damaged    = "test-png-damaged.png"
    it "Original vs Scaled Down - same" $ do
      r <- avgDistance orig scaledDown
      r `shouldBe` Right 0
    it "Original vs Damaged - 1%" $ do
      r <- avgDistance orig damaged
      r `shouldBe` Right 3.125
    it "Scaled Down vs Damaged - 1%" $ do
      r <- avgDistance scaledDown damaged
      r `shouldBe` Right 3.125
    it "Not existed file, thorow exception" $ do
      r <- avgDistance "not-existed-file.png" damaged
      r
        `shouldBe` Left
                    "not-existed-file.png: openBinaryFile: does not exist (No such file or directory)"
