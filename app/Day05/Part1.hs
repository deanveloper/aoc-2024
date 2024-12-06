module Day05.Part1 where

import Data.List
import Data.Maybe
import Data.List.Split

run :: String -> Int
run contents =
    let
        (ruleLines, pageLines) = splitParts contents
        rules = map parseRule ruleLines
        pages = map parsePages pageLines
    in
        sum $ map middlePage $ filter (pagesFollowRules rules) pages

middlePage :: [Int] -> Int
middlePage pages = pages !! (length pages `div` 2)

-- splits contents into (ruleLines, pageLines)
splitParts :: String -> ([String], [String])
splitParts contents =
    let
        contentsLines = lines contents
        emptyLine = fromJust $ elemIndex "" contentsLines
    in  (take emptyLine contentsLines, drop (emptyLine+1) contentsLines)

parseRule :: String -> (Int, Int)
parseRule rule =
    let
        pipe = fromJust $ elemIndex '|' rule
    in  (read $ take pipe rule, read $ drop (pipe+1) rule)

parsePages :: String -> [Int]
parsePages = map read . splitOn ","

pagesFollowRules :: [(Int, Int)] -> [Int] -> Bool
pagesFollowRules rules pages =
        let (success, _) = foldr (pageFollowRulesReducer rules) (True, []) pages
        in  success

pageFollowRulesReducer :: [(Int, Int)] -> Int -> (Bool, [Int]) -> (Bool, [Int])
pageFollowRulesReducer rules page (matches, after)
  | not matches = (False, [])
  | any (\ (l, r) -> r == page && l `elem` after) rules = (False, [])
  | otherwise = (True, page:after)
