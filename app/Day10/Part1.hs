module Day10.Part1 where

import Debug.Trace
import Shared (parseGridInts)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (nub)

run :: String -> Int
run contents = let m = parseTopographicMap contents in trace (show (search (1,7) m)) sum $ (`score` m) <$> startingPoints m

data Cardinal = North | East | South | West

data TopographicMap = TopographicMap {
    dimensions :: (Int, Int),
    mapData :: [[Int]]
}

startingPoints :: TopographicMap -> [(Int, Int)]
startingPoints m = foldr (\(y, r) total -> total ++ foldr (\(x, a) rowAcc -> if a == 0 then (x, y):rowAcc else rowAcc) [] (zip [0..] r)) [] (zip [0..] $ mapData m)

score :: (Int, Int) -> TopographicMap -> Int
score c m = length $ nub $ last <$> search c m

search :: (Int, Int) -> TopographicMap -> [[(Int, Int)]]
search = search' Set.empty

search' :: Set.Set (Int, Int) -> (Int, Int) -> TopographicMap -> [[(Int, Int)]]
search' v c m
  | c `Set.member` v = []
  | altitude c m == 9 = [[c]]
  | otherwise = concatMap
        (\c' -> (\p -> if null p then [] else c:p) <$> search' (Set.insert c v) c' m)
        (mapMaybe (\d -> travel d c m) [North, East, South, West])

altitude :: (Int, Int) -> TopographicMap -> Int
altitude (x, y) m = mapData m !! y !! x

parseTopographicMap :: String -> TopographicMap
parseTopographicMap content = let (d, m) = parseGridInts content in TopographicMap{ dimensions = d, mapData = m }

travelRaw :: Cardinal -> (Int, Int) -> TopographicMap -> Maybe (Int, Int)
travelRaw North (x, y) m = if x <= 0 || x >= fst (dimensions m) + 1 then Nothing else Just (x-1, y)
travelRaw East (x, y) m = if y < -1 || y >= snd (dimensions m) - 1 then Nothing else Just (x, y+1)
travelRaw South (x, y) m = if x < -1 || x >= fst (dimensions m) - 1 then Nothing else Just (x+1, y)
travelRaw West (x, y) m = if y <= 0 || y >= fst (dimensions m) + 1 then Nothing else Just (x, y-1)

travel ::  Cardinal -> (Int, Int) -> TopographicMap -> Maybe (Int, Int)
travel d c m = case travelRaw d c m of
    Just c' -> if (altitude c m - altitude c' m) == -1 then Just c' else Nothing
    Nothing -> Nothing

