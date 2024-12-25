module Shared where

parseGridChars :: String -> ((Int, Int), [[Char]])
parseGridChars content = let rows = lines content in ((length $ head rows, length rows), rows)

parseGridInts :: String -> ((Int, Int), [[Int]])
parseGridInts content = let (dims, gridChars) = parseGridChars content in (dims, map (\s -> (\c -> read [c]) <$> s) gridChars)
