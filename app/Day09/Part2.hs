module Day09.Part2 where

import Data.List (findIndex)
import Data.Maybe (fromJust)

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

diskMapRemove :: Int -> DiskMap -> DiskMap
diskMapRemove idx diskMap = let
        entry = diskMap !! idx
        entryBefore = diskMap !! (idx - 1)
        entryBefore' = entryBefore { freeSpace = freeSpace entryBefore + fileSize entry + freeSpace entry }
    in  take (idx-1) diskMap ++ [entryBefore'] ++ drop (idx+1) diskMap

diskMapInsertAfter :: Int -> DiskMapEntry -> DiskMap -> DiskMap
diskMapInsertAfter idx entry diskMap = let
        entryBefore = diskMap !! idx
        entryBefore' = entryBefore{ freeSpace = 0 }
        entry' = entry{ freeSpace = freeSpace entryBefore - fileSize entry }
    in  take idx diskMap ++ [entryBefore', entry'] ++ drop (idx+1) diskMap

defragFile :: Int -> DiskMap -> DiskMap
defragFile idToDefrag diskMap = let
        entryIdx = findIndex (\e -> fileId e == idToDefrag) diskMap
        entry = ((diskMap !!) <$> entryIdx)
        freeEntryIdx = (\e -> findIndex (\(i, each) -> fileSize e <= freeSpace each && i < fromJust entryIdx) (indexed diskMap)) =<< entry
    in 
        case (entryIdx, freeEntryIdx) of
            (Just eIdx, Just feIdx) -> (diskMapInsertAfter feIdx (fromJust entry). diskMapRemove eIdx) diskMap
            _ -> diskMap

slice :: Int -> Int -> [a] -> [a]
slice from to list = take (to-from) (drop from list)

indexed :: [a] -> [(Int, a)]
indexed = indexed' 0

indexed' :: Int -> [a] -> [(Int, a)]
indexed' _ [] = []
indexed' count list = (count, head list):indexed' (count+1) (tail list)
