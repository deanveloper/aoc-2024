module Day02.Part2 where

import Data.List

run :: String -> Int
run contents = 
    let
        reports = [parseReport line | line <- lines contents]
        safeReports = filter isSafeWithRetry reports
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

isSafeWithRetry :: [Int] -> Bool
isSafeWithRetry report = 
    let
        diffs = diffsOf report
        (unsafeAsc, unsafeDesc) = firstUnsafe diffs
    in  case (unsafeAsc, unsafeDesc) of
        (Just _, Nothing) -> True
        (Nothing, Just _) -> True
        (Just unsafeAscIdx, Just unsafeDescIdx) -> (isSafe (without unsafeAscIdx report)) || (isSafe (without (unsafeAscIdx + 1) report)) || (isSafe (without unsafeDescIdx report)) || (isSafe (without (unsafeDescIdx + 1) report))
        (Nothing, Nothing) -> True

isSafe :: [Int] -> Bool
isSafe report =
    let
        diffs = diffsOf report
        (unsafeAsc, unsafeDesc) = firstUnsafe diffs
    in  case (unsafeAsc, unsafeDesc) of
        (Just _, Nothing) -> True
        (Nothing, Just _) -> True
        (Just _, Just _) -> False
        (Nothing, Nothing) -> True


without :: Int -> [a] -> [a]
without idx list = (take idx list) ++ (drop (idx + 1) list)

firstUnsafe :: [Int] -> (Maybe Int, Maybe Int)
firstUnsafe diffs = (findIndex (\each -> each < 1 || 3 < each) diffs, findIndex (\each -> each < -3 || -1 < each) diffs)

diffsOf :: [Int] -> [Int]
diffsOf [] = []
diffsOf [_] = []
diffsOf list =
    let
        (first, rest) = (head list, tail list)
    in  (first - (head rest)):(diffsOf rest)
