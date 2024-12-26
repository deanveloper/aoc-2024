module Day11.Part2 where

import Shared (parseIntList)
import qualified Data.Map as Map

run :: String -> Int
run = sum . Map.elems. (!! 75) . iterate applyStoneRules . intListToMap . parseIntList

intListToMap :: [Int] -> Map.Map Int Int
intListToMap = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

applyStoneRules :: Map.Map Int Int -> Map.Map Int Int
applyStoneRules = Map.foldrWithKey applyRules Map.empty

applyRules :: Int -> Int -> Map.Map Int Int -> Map.Map Int Int
applyRules 0 n = Map.insertWith (+) 1 n
applyRules stone n
    | even digitCount = let (l, r) = splitNumber stone in Map.insertWith (+) l n . Map.insertWith (+) r n
    | otherwise = Map.insertWith (+) (stone * 2024) n
    where
        asStr = show stone :: String
        digitCount = length asStr

splitNumber :: Int -> (Int, Int)
splitNumber stone = (read $ take (digitCount `div` 2) asStr, read $ drop (digitCount `div` 2) asStr)
    where
        asStr = show stone :: String
        digitCount = length asStr
