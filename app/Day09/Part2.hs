module Day09.Part2 where

-- THIS SOLUTION DOES NOT WORK AND IM NOT SURE WHY :(

import Data.List (findIndex)
import Data.Maybe (isJust, fromJust)

run :: String -> Int
run = checksum . compact . defrag . parseDiskMap

data DiskMapEntry = DiskMapEntry { fileId :: Int, fileSize :: Int, freeSpace :: Int }
instance Show DiskMapEntry where
    show value = "{" ++ replicate (fileSize value) (head $ show $ fileId value) ++ replicate (freeSpace value) '.' ++ "}"
type DiskMap = [DiskMapEntry]

parseDiskMap :: String -> DiskMap
parseDiskMap = parseDiskMap' 0

parseDiskMap' :: Int -> String -> DiskMap
parseDiskMap' _ "" = []
parseDiskMap' idx (size':freeSpace':rest) = DiskMapEntry{ fileId = idx, fileSize = read [size'], freeSpace = read [freeSpace'] }:parseDiskMap' (idx+1) rest
parseDiskMap' idx [size'] = [DiskMapEntry{ fileId = idx, fileSize = read [size'], freeSpace = 0 }]

type CompactDiskMap = [Int]
compact :: DiskMap -> CompactDiskMap
compact = foldr (\ f -> (++) (replicate (fileSize f) (fileId f) ++ replicate (freeSpace f) 0)) []

checksum :: CompactDiskMap -> Int
checksum = checksum' 0

checksum' :: Int -> CompactDiskMap -> Int
checksum' _ [] = 0
checksum' idx (first:rest) = idx*first + checksum' (idx+1) rest

defrag :: DiskMap -> DiskMap
defrag diskMap = defrag' (foldr (max . fileId) 0 diskMap) diskMap

defrag' :: Int -> DiskMap -> DiskMap
defrag' (-1) diskMap = diskMap
defrag' idToDefrag diskMap = defrag' (idToDefrag - 1) $ defragFile idToDefrag diskMap

defragFile :: Int -> DiskMap -> DiskMap
defragFile idToDefrag diskMap = let
        entryIdx = findIndex (\e -> fileId e == idToDefrag) diskMap
        entry = (diskMap !!) <$> entryIdx
        beforeEntry = (\idx -> if idx == 0 then Nothing else Just (diskMap !! (idx - 1))) =<< entryIdx
        freeEntryIdx = (\e -> findIndex (\(i, each) -> fileSize e <= freeSpace each && i < fromJust entryIdx) (indexed diskMap)) =<< entry
        freeEntry = (diskMap !!) <$> freeEntryIdx
        freeEntry' = (\e -> e{ freeSpace = 0 }) <$> freeEntry
        entry' = (\e -> e{ freeSpace = freeSpace (fromJust freeEntry) - fileSize e}) <$> entry
        beforeEntry' = (\e -> e{ freeSpace = freeSpace e + fileSize (fromJust entry) + freeSpace (fromJust entry)}) <$> beforeEntry
    in
        if all isJust [freeEntry', entry', beforeEntry'] then let
                untouchedBefore = take (fromJust freeEntryIdx) diskMap
                replaceFreeSpace = fromJust <$> [freeEntry', entry']
                untouchedBetween = slice (fromJust freeEntryIdx + 1) (fromJust entryIdx - 1) diskMap
                replaceDefragged = [fromJust beforeEntry']
                untouchedAfter = drop (fromJust entryIdx + 1) diskMap
            in
                untouchedBefore ++ replaceFreeSpace ++ untouchedBetween ++ replaceDefragged ++ untouchedAfter
        else diskMap

slice :: Int -> Int -> [a] -> [a]
slice from to list = take (to-from) (drop from list)

indexed :: [a] -> [(Int, a)]
indexed = indexed' 0

indexed' :: Int -> [a] -> [(Int, a)]
indexed' _ [] = []
indexed' count list = (count, head list):indexed' (count+1) (tail list)
