module Day03.Part2 where

import Data.List
import Text.Read

run :: String -> Int

run contents = 
    let
        numberPairs = muls (withoutDisabled contents)
    in  sum (map (uncurry (*)) numberPairs)

withoutDisabled :: String -> String
withoutDisabled contents = case indexOf "don't()" contents of
    Just dont -> let
            afterDont = dont+7
        in
            take dont contents ++ skipDisabled (drop afterDont contents)
    Nothing -> contents

skipDisabled :: String -> String
skipDisabled contents = case indexOf "do()" contents of
    Just doCall -> let afterDo = doCall+4 in withoutDisabled (drop afterDo contents)
    Nothing -> []

muls :: String -> [(Int, Int)]
muls contents = case indexOf "mul" contents of
    Just index -> let
            restContents = drop (index+3) contents
            endIdx = elemIndex ')' restContents
            numsStr = ((\end -> Just (take (end+1) restContents)) =<< endIdx)
            nums = (readMaybe =<< numsStr)
        in  maybe (muls restContents) (\n -> n:muls restContents) nums
    Nothing -> []

indexOf :: String -> String -> Maybe Int
indexOf _ "" = Nothing
indexOf needle haystack = if startsWith needle haystack then Just 0 else (\i -> Just (i + 1)) =<< indexOf needle (tail haystack)

startsWith :: String -> String -> Bool
startsWith "" _ = True
startsWith needle haystack = (head needle == head haystack) && startsWith (tail needle) (tail haystack)