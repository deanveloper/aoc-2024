module Day04.Part2 where

import Debug.Trace

run :: String -> Int
run contents = masCount (lines contents)
 
masCount :: [String] -> Int
masCount search = masCount' 0 search

masCount' :: Int -> [String] -> Int
masCount' row search = if row >= length search then 0 else (masInRow row search) + (masCount' (row + 1) search)

masInRow :: Int -> [String] -> Int
masInRow rowIdx search = masInRow' (0, rowIdx) search

masInRow' :: (Int, Int) -> [String] -> Int
masInRow' (x, y) search = if x >= length (search !! y) then 0 else masCountAt (x, y) search + masInRow' (x+1, y) search

masCountAt :: (Int, Int) -> [String] -> Int
masCountAt start search = foldl (\ acc each -> acc + if each start search then 1 else 0) 0 [masN, masE, masS, masW]

masN :: (Int, Int) -> [String] -> Bool
masN (x, y) search = (spellsMas (x+1, y-1) (x, y) (x-1, y+1) search) && (spellsMas (x-1, y-1) (x, y) (x+1, y+1) search)

masE :: (Int, Int) -> [String] -> Bool
masE (x, y) search = (spellsMas (x-1, y-1) (x, y) (x+1, y+1) search) && (spellsMas (x-1, y+1) (x, y) (x+1, y-1) search)

masS :: (Int, Int) -> [String] -> Bool
masS (x, y) search = (spellsMas (x-1, y+1) (x, y) (x+1, y-1) search) && (spellsMas (x+1, y+1) (x, y) (x-1, y-1) search)

masW :: (Int, Int) -> [String] -> Bool
masW (x, y) search = (spellsMas (x+1, y+1) (x, y) (x-1, y-1) search) && (spellsMas (x+1, y-1) (x, y) (x-1, y+1) search)

spellsMas :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [String] -> Bool
spellsMas c1 c2 c3 search = [at c1 search, at c2 search, at c3 search] == [Just 'M', Just 'A', Just 'S']

at :: (Int, Int) -> [String] -> Maybe Char
at (x, y) search =
    if x < 0 || y < 0 then Nothing
    else if y >= length search then Nothing
    else let row = search !! y in
        if x >= length row then Nothing
        else Just (row !! x)
