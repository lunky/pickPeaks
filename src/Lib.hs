module Lib
    ( someFunc
    , pickPeaks
    , findPeaks
    , PickedPeaks(..)
    ) where
data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)

pickPeaks :: [Int] -> PickedPeaks
pickPeaks [] = PickedPeaks [] []
pickPeaks (x:xs) = (\(peak,_,pos)->PickedPeaks (map (+1) pos) peak) 
    $ unzip3 $ findPeaks (x,0) (zip3 xs (tail xs ++ [last xs]) [1..])

findPeaks:: (Int,Int) -> [(Int,Int,Int)] -> [(Int,Int,Int)]
findPeaks s [] = []
findPeaks (prev,prevIdx) ((curr,next,idx):acc)
    | prev < curr && curr > next  = (curr,next,prevIdx) : findPeaks (curr,idx) acc
    | curr < next =  findPeaks (curr,idx) acc
    | otherwise =  findPeaks (prev,prevIdx) acc
    
-- your code here
someFunc :: IO ()
someFunc = putStrLn "someFunc"
