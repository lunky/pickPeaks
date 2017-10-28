module Lib
    ( someFunc
    , findPeaks
    , pickPeaks
    , PickedPeaks(..)
    ) where
data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)

pickPeaks :: [Int] -> PickedPeaks
--pickPeaks a = PickedPeaks a a--set =filter (\(x, index) -> x > (average set))  zip set [1..]
pickPeaks [] = PickedPeaks [] [] --filter (\(x, index) -> x > (average set))  zip set [1..]
pickPeaks set = wrap $ 
                    unzip $
                    map (\((_,peak,_),idx)-> (peak,idx)) $ 
                    filter (\((x,y,z),idx) -> y > x && y > z) $  
                    zip (tri set) [1..]
    where dedup = filter(\(x,y) -> x /= y) $ zip set $ tail set
          wrap s = PickedPeaks (snd s) (fst s)
          tri a = zip3 a (tail a) ( tail $ tail a )

-- let set =  [3,2,3,6,4,1,2,3,2,1,2,3]
-- let dedup = filter (\(x,y,idx) -> x /= y) $ zip3 set (tail set) [0..]

-- below works but not with plateaus
-- filter (\((x,y,z),idx) -> y > x && y > z)$  zip (tri set) [1..]
-- map (\((_,peak,_),idx)-> (peak,idx)) $ filter (\((x,y,z),idx) -> y > x && y > z)$  zip (tri set) [1..]





findPeaks :: (Int, Int) -> [(Int,Int)] -> [(Int,Int)]
findPeaks (x, index) [] = [(x,index)]
findPeaks (x, index) ((y,yindex):acc) = if ( x > y ) then ((x,index):acc) else ((y,yindex):acc) 


--let set2 = zipWith3 (\y x z -> (x-y, z)) set  (tail set) [1..] 
-- your code here
someFunc :: IO ()
someFunc = putStrLn "someFunc"
