module Day04.Part1 where

run :: String -> Int
run contents = xmasCount (lines contents)
 
xmasCount :: [String] -> Int
xmasCount search = xmasCount' 0 search

xmasCount' :: Int -> [String] -> Int
xmasCount' row search = if row >= length search then 0 else (xmasInRow row search) + (xmasCount' (row + 1) search)

xmasInRow :: Int -> [String] -> Int
xmasInRow rowIdx search = xmasInRow' (0, rowIdx) search

xmasInRow' :: (Int, Int) -> [String] -> Int
xmasInRow' (x, y) search = if x >= length (search !! y) then 0 else xmasCountAt (x, y) search + xmasInRow' (x+1, y) search

xmasCountAt :: (Int, Int) -> [String] -> Int
xmasCountAt start search = foldl (\ acc each -> acc + if each start search then 1 else 0) 0 [xmasN, xmasNE, xmasE, xmasSE, xmasS, xmasSW, xmasW, xmasNW]

xmasN :: (Int, Int) -> [String] ->  Bool
xmasN (x, y) search = spellsXmas (x, y) (x, y-1) (x, y-2) (x, y-3) search

xmasNE :: (Int, Int) -> [String] -> Bool
xmasNE (x, y) search = spellsXmas (x, y) (x+1, y-1) (x+2, y-2) (x+3, y-3) search

xmasE :: (Int, Int) -> [String] -> Bool
xmasE (x, y) search = spellsXmas (x, y) (x+1, y) (x+2, y) (x+3, y) search

xmasSE :: (Int, Int) -> [String] -> Bool
xmasSE (x, y) search = spellsXmas (x, y) (x+1, y+1) (x+2, y+2) (x+3, y+3) search

xmasS :: (Int, Int) -> [String] -> Bool
xmasS (x, y) search = spellsXmas (x, y) (x, y+1) (x, y+2) (x, y+3) search

xmasSW :: (Int, Int) -> [String] -> Bool
xmasSW (x, y) search = spellsXmas (x, y) (x-1, y+1) (x-2, y+2) (x-3, y+3) search

xmasW :: (Int, Int) -> [String] -> Bool
xmasW (x, y) search = spellsXmas (x, y) (x-1, y) (x-2, y) (x-3, y) search

xmasNW :: (Int, Int) -> [String] -> Bool
xmasNW (x, y) search = spellsXmas (x, y) (x-1, y-1) (x-2, y-2) (x-3, y-3) search

spellsXmas :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [String] -> Bool
spellsXmas c1 c2 c3 c4 search = [at c1 search, at c2 search, at c3 search, at c4 search] == [Just 'X', Just 'M', Just 'A', Just 'S']

at :: (Int, Int) -> [String] -> Maybe Char
at (x, y) search =
    if x < 0 || y < 0 then Nothing
    else if y >= length search then Nothing
    else let row = search !! y in
        if x >= length row then Nothing
        else Just (row !! x)
