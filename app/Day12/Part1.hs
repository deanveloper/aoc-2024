module Day12.Part1 where

import Shared (parseGridChars)
import qualified Data.Set as Set
import Debug.Trace

run :: String -> Int
run = uncurry price . parseGarden . parseGridChars

parseGarden :: ((Int, Int), [[Char]]) -> ((Int, Int), Regions)
parseGarden =
    (\gp -> ((width gp, height gp), regions gp)) . (\gp -> foldr visit gp [(x, y) | x <- [0..width gp - 1], y <- [0..height gp - 1]]) . uncurry initGardenParser

data GardenParser = GardenParser {
    width :: Int,
    height :: Int,
    grid :: [[Char]],
    regions :: [(Char, Set.Set (Int, Int))],
    seen :: Set.Set (Int, Int)
} deriving Eq
gridAt :: (Int, Int) -> GardenParser -> Char
gridAt (x, y) = (!! x) . (!! y) . grid

initGardenParser :: (Int, Int) -> [[Char]] -> GardenParser
initGardenParser (w, h) g = GardenParser{ width = w, height = h, grid = g, regions = [], seen = Set.empty }

visit :: (Int, Int) -> GardenParser -> GardenParser
visit (x, y) gp
    | Set.member (x, y) $ seen gp = trace ("visiting " ++ show (x, y)) gp
    | otherwise = trace ("visiting " ++ show (x, y)) gp{ seen = Set.union (seen gp) region, regions = (c, region):regions gp }
    where
        c = gridAt (x, y) gp
        region = glob (x, y) c Set.empty gp

glob :: (Int, Int) -> Char -> Set.Set (Int, Int) -> GardenParser -> Set.Set (Int, Int)
glob (x, y) c acc gp
    | outOfBounds (x, y) (width gp, height gp) = acc
    | Set.member (x, y) acc = acc
    | gridAt (x, y) gp /= c = acc
    | otherwise = let
            north = (x, y-1)
            east = (x+1, y)
            south = (x, y+1)
            west = (x-1, y)
        in
            foldr (\e acc' -> glob e c (Set.unions [acc, acc', Set.singleton (x, y)]) gp) Set.empty [north, east, south, west] 

type Regions = [(Char, Set.Set (Int, Int))]

price :: (Int, Int) -> Regions -> Int
price dims = foldr (\(c, r) acc -> trace ("price for " ++ [c] ++ " is " ++ show (length r) ++ " * " ++ show (perimeter dims r) ++ " = " ++ show (perimeter dims r * length r)) acc + (perimeter dims r * length r)) 0

perimeter :: (Int, Int) -> Set.Set (Int, Int) -> Int
perimeter dims s = sum $ map (\c -> borders c dims s) $ Set.toList s

borders :: (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> Int
borders (x, y) dims s = length $ filter id $ (\c -> c `Set.notMember` s || outOfBounds c dims) <$> [north, east, south, west]
    where
        north = (x, y-1)
        east = (x+1, y)
        south = (x, y+1)
        west = (x-1, y)

outOfBounds :: (Int, Int) -> (Int, Int) -> Bool
outOfBounds (x, y) (w, h) = x < 0 || y < 0 || x >= w || y >= h

applyRules :: Int -> [Int]
applyRules 0 = [1]
applyRules stone
    | even digitCount = [read $ take (digitCount `div` 2) asStr, read $ drop (digitCount `div` 2) asStr]
    | otherwise = [stone * 2024]
    where
        asStr = show stone :: String
        digitCount = length asStr
