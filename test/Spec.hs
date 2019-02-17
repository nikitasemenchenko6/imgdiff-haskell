import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Format
import Lib
import System.IO (putStrLn)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $ do
    describe "Lib.distance" $ do
      let orig = "test-png-original.png"
      let scaledDown = "test-png-scaled-down.png"
      let damaged = "test-png-damaged.png"
      it "Original vs Scaled Down - same" $ do
        r <- distance orig scaledDown
        r `shouldBe` 0
      it "Original vs Damaged - 1%" $ do
        r <- distance orig damaged
        r `shouldBe` 3
      it "Scaled Down vs Damaged - 1%" $ do
        r <- distance scaledDown damaged
        r `shouldBe` 3
      it "Not existed file, thorow exception" $ do
        (evaluate . force =<< (distance "not-existed-file.png" damaged)) `shouldThrow` anyException