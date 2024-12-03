module Main where

import Day02.Part2

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print (run contents)
