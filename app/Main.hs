module Main where

import Day09.Part2
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ run (trim contents)

trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace
