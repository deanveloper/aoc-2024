module Day09.Part1 where

import Debug.Trace

run :: String -> Int
run = checksum . compact . parseDiskMap

data DiskMapEntry = DiskMapEntry { fileId :: Int, fileSize :: Int, freeSpace :: Int } deriving Show
type DiskMap = [DiskMapEntry]

parseDiskMap :: String -> DiskMap
parseDiskMap = parseDiskMap' 0

parseDiskMap' :: Int -> String -> DiskMap
parseDiskMap' _ "" = []
parseDiskMap' idx (size':freeSpace':rest) = DiskMapEntry{ fileId = idx, fileSize = read [size'], freeSpace = read [freeSpace'] }:parseDiskMap' (idx+1) rest
parseDiskMap' idx [size'] = [DiskMapEntry{ fileId = idx, fileSize = read [size'], freeSpace = 0 }]

type CompactDiskMap = [Int]
compact :: DiskMap -> CompactDiskMap
compact [] = []
compact (firstEntry:rest) =
    let (freeSpaceFiller, restAfterFilling) = fillFreeSpace (freeSpace firstEntry) rest
    in replicate (fileSize firstEntry) (fileId firstEntry) ++ freeSpaceFiller ++ compact restAfterFilling

checksum :: CompactDiskMap -> Int
checksum = checksum' 0

checksum' :: Int -> CompactDiskMap -> Int
checksum' _ [] = 0
checksum' idx (first:rest) = idx*first + checksum' (idx+1) rest

fillFreeSpace :: Int -> DiskMap -> ([Int], DiskMap)
fillFreeSpace space diskMap
    | space == 0 || null diskMap = ([], diskMap)
    | lastSize < space = let (restFreeSpace, remainingDiskMap) = fillFreeSpace (space-lastSize) (init diskMap) in (replicate lastSize (fileId lastEntry) ++ restFreeSpace, remainingDiskMap) -- TODO, incorrect
    | lastSize == space = (replicate lastSize (fileId lastEntry), init diskMap)
    | lastSize > space = (replicate space (fileId lastEntry), init diskMap ++ [lastEntry{ fileSize = lastSize - space }])
    | otherwise = error "Impossible"
    where
        lastEntry = last diskMap
        lastSize = fileSize lastEntry
