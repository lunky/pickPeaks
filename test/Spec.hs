import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Sample test" $ do
        it "[3,2,3,6,4,1,2,3,2,1,2,3]" $ do
            let myPeak = PickedPeaks {pos = [3,7], peaks = [6,3]}
            pickPeaks [3,2,3,6,4,1,2,3,2,1,2,3] `shouldBe` myPeak        
        it "[3,2,3,6,4,8,2,3,2,1,2,3]" $ do
            let myPeak = PickedPeaks {pos = [3,5,7], peaks = [6,8,3]}
            pickPeaks [3,2,3,6,4,8,2,3,2,1,2,3] `shouldBe` myPeak        
--        it "[1,2,2,1]" $ do
--            let myPeak = PickedPeaks {pos = [1], peaks = [2]}
--            pickPeaks [1,2,2,1] `shouldBe` myPeak        