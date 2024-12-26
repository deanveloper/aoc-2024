module Day11.Part1 where
import Shared (parseIntList)

run :: String -> Int
run = length . (!! 25) . iterate applyStoneRules . parseIntList

applyStoneRules :: [Int] -> [Int]
applyStoneRules = concatMap applyRules

applyRules :: Int -> [Int]
applyRules 0 = [1]
applyRules stone
    | even digitCount = [read $ take (digitCount `div` 2) asStr, read $ drop (digitCount `div` 2) asStr]
    | otherwise = [stone * 2024]
    where
        asStr = show stone :: String
        digitCount = length asStr
