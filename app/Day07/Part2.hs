module Day07.Part2 where

import Data.Maybe (isJust, mapMaybe, fromMaybe)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Data.Foldable (find)

run :: String -> Int
run contents =
    let
        equations = map parseEquation $ lines contents
        goodEquations = filter (\ (_, maybeOps) -> isJust maybeOps) $ map (\ (target, nums) -> (target, solveOperators target nums)) equations
        goodTargets = map fst goodEquations
    in  sum goodTargets

parseEquation :: String -> (Int, [Int])
parseEquation line =
        case splitOn ":" line of
            targetStr:numsStr:_ -> (read targetStr, mapMaybe readMaybe $ splitOn " " numsStr)
            [_] -> (-1, [])
            [] -> (-1, [])

data Op = Plus | Times | Concat
asFunc :: Op -> Int -> Int -> Int
asFunc Plus = (+)
asFunc Times = (*)
asFunc Concat = concatInt

-- takes a target, list of numbers, and returns a list of binary operators.
solveOperators :: Int -> [Int] -> Maybe [Op]
solveOperators _ [] = Nothing
solveOperators target [a] = if target == a then Just [] else Nothing
solveOperators target (x:y:rest) =
    if x > target then Nothing
    else fromMaybe Nothing $
        find isJust $
            map (\ op -> solveOperators target $ asFunc op x y : rest) [Plus, Times, Concat]

concatInt :: Int -> Int -> Int
concatInt a b = a * base10flatten b + b

base10flatten :: Int -> Int
base10flatten a = base10flatten' a 1

base10flatten' :: Int -> Int -> Int
base10flatten' a digit = if digit > a then digit else base10flatten' a (digit * 10)
