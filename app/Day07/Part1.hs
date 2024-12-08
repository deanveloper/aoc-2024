module Day07.Part1 where

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

data Op = Plus | Times
asFunc :: Op -> Int -> Int -> Int
asFunc Plus = (+)
asFunc Times = (*)

-- takes a target, list of numbers, and returns a list of binary operators.
solveOperators :: Int -> [Int] -> Maybe [Op]
solveOperators _ [] = Nothing
solveOperators target [a] = if target == a then Just [] else Nothing
solveOperators target (x:y:rest) =
    if x > target then Nothing
    else fromMaybe Nothing $
        find isJust $
            map (\ op -> solveOperators target $ asFunc op x y : rest) [Plus, Times]
