module Day02.Part1 where

import Data.List

run :: String -> Int
run contents = 
    let
        reports = [parseReport line | line <- lines contents]
        safeReports = filter isSafe reports
    in  length safeReports

parseReport :: String -> [Int]
parseReport line =
    case (elemIndex ' ' line) of
        Just space -> 
            let
                (thisStr, rest) = splitAt space line
                this = (read thisStr :: Int)
            in  this:(parseReport (tail rest))
        Nothing -> [read line :: Int]

isSafe :: [Int] -> Bool
isSafe report =
    let
        diffs = diffsOf report
    in  (all (\each -> 1 <= each && each <= 3) diffs) || (all (\each -> -3 <= each && each <= -1) diffs)

diffsOf :: [Int] -> [Int]
diffsOf [] = []
diffsOf [_] = []
diffsOf list =
    let
        (first, rest) = (head list, tail list)
    in  (first - (head rest)):(diffsOf rest)

