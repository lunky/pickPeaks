module Lib
    ( someFunc
    
    , pickPeaks
    , average
    
    ) where
data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)

pickPeaks :: [Int] -> PickedPeaks
pickPeaks a = PickedPeaks a a--set =filter (\(x, index) -> x > (average set))  zip set [1..]

average :: (Fractional a, Foldable t) => t a -> a
average set = (sum set) / fromIntegral (length set)

-- your code here
someFunc :: IO ()
someFunc = putStrLn "someFunc"
