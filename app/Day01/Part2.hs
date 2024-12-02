module Day01.Part2 where

import Data.List

run :: String -> Int

run contents =
    let numberPairs = map pairs  (lines contents)
        transmuted = unzip numberPairs :: ([Int], [Int])
        secondSorted = sort (snd transmuted)
        similarities = map (\each -> (each, similarityOf each secondSorted)) (fst transmuted)
        products = map (uncurry (*)) similarities
    in  sum products

pairs :: String -> (Int, Int)
pairs line = 
    let spaceIdx = case (elemIndex ' ' line) of
            Just value -> value
            Nothing -> 0
        intStrs = splitAt spaceIdx line
    in  (read (fst intStrs), read (snd intStrs))

similarityOf :: Int -> [Int] -> Int
similarityOf i list = length (filter (\each -> each == i) list)
