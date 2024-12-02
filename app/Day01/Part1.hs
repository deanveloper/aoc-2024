module Day01.Part1 where

import Data.List

run :: String -> Int

run contents =
    let
        numberPairs = map pairs (lines contents)
        transmuted = unzip numberPairs :: ([Int], [Int])
        sortedLists = (sort (fst transmuted), sort (snd transmuted))
        sorted = zip (fst sortedLists) (snd sortedLists)
        diffs = map (\(a, b) -> abs (a - b)) sorted
    in  sum diffs

pairs :: String -> (Int, Int)
pairs line = (read (take 5 line) :: Int, read (drop 6 line) :: Int)

