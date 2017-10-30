import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "findPeaks" $ do 
        it "[3,2,3,6,4,1,2,3,2,1,2,3]" $ do
            let set = [3,2,3,6,4,1,2,3,2,1,2,3] 
            findPeaks ((head set),0) (zip3 set (tail set) [1..])  `shouldBe` [(6,4,3),(3,2,7)]
--        it "[1,2,2,1]" $ do
--            let set = [1,2,2,1]
--            peakTest ((head set),0) (zip3 set (tail set) [0..])  `shouldBe` [(2,1,2)]
    describe "Sample test" $ do
        it "[3,2,3,6,4,1,2,3,2,1,2,3]" $ do
            let myPeak = PickedPeaks {pos = [3,7], peaks = [6,3]}
            pickPeaks [3,2,3,6,4,1,2,3,2,1,2,3] `shouldBe` myPeak        
        it "[3,2,3,6,4,8,2,3,2,1,2,3]" $ do
            let myPeak = PickedPeaks {pos = [3,5,7], peaks = [6,8,3]}
            pickPeaks [3,2,3,6,4,8,2,3,2,1,2,3] `shouldBe` myPeak        
        it "[1,2,2,1]" $ do
            let myPeak = PickedPeaks {pos = [1], peaks = [2]}
            pickPeaks [1,2,2,1] `shouldBe` myPeak        
        it "[1,2,2,2,2,2,2,1]" $ do
            let myPeak = PickedPeaks {pos = [1], peaks = [2]}
            pickPeaks [1,2,2,2,2,2,2,1] `shouldBe` myPeak        
        it "[0, 1, 2, 5, 1, 0]" $ do 
            let myPeak = PickedPeaks {pos = [3], peaks = [5]}
            pickPeaks [0, 1, 2, 5, 1, 0] `shouldBe` myPeak
        it "[0,2,2,2]" $ do 
            let myPeak = PickedPeaks {pos = [], peaks = []}
            pickPeaks [0, 2, 2, 2] `shouldBe` myPeak