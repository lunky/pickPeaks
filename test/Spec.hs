import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "average" $ do 
        it "should average 3 items" $ do 
            average [1, 2, 3] `shouldBe` 2