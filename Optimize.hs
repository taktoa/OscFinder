module Optimize where
import Data.List.Key (sort)
import Data.List (group)

makeList :: (Double, Double) -> [Int] -> [Double]
makeList range vals = if low < high then map (* low) values ++ makeList (low * 10, high) vals else []
        where
        low = fst range
        high = snd range
        values = map fromIntegral vals

makeSet :: [[Double]] -> ([Double] -> (Double, [Double])) -> Double -> [(Double, [Double])]
makeSet x f freq = head . group . sort fst $ map ((\ b -> (abs (fst b - freq), snd b)) . f) (sequence x)

findBest :: [(Double, Double)] -> [Int] -> ([Double] -> (Double, [Double])) -> Double -> [(Double, [Double])]
findBest ranges preclist = makeSet (zipWith makeList ranges (replicate (length ranges) preclist))

findBest' = makeSet