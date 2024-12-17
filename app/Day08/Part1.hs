module Day08.Part1 where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Sequence


run :: String -> Int
run contents = let grid = parseGrid contents in length $ Map.foldr Set.union Set.empty $ antinodes grid

data Grid = Grid {
    dims :: (Int, Int),
    nodes :: Map.Map Char (Set.Set (Int, Int)),
    antinodes :: Map.Map Char (Set.Set (Int, Int))
} deriving Show

parseGrid :: String -> Grid
parseGrid content =
    let
        contentLines = lines content
        gridDims = (length $ head contentLines, length contentLines)
        nodesByRow = map nodesInRow contentLines
        gridNodes = Sequence.foldrWithIndex
                    (\y row acc ->
                        Map.unionWith Set.union acc $ Map.map (Set.map (\x -> (x, y))) row)
                    Map.empty
                    $ Sequence.fromList nodesByRow
    in removeOutOfBounds $ Map.foldrWithKey (\ freq nodesAtFreq grid ->
            Set.foldr (\ node grid' -> withNode grid' freq node)
            grid
            nodesAtFreq
        ) Grid{ dims = gridDims, nodes = Map.empty, antinodes = Map.empty } gridNodes

removeOutOfBounds :: Grid -> Grid
removeOutOfBounds grid = grid{
    antinodes = let
            width = fst $ dims grid
            height = snd $ dims grid
        in Map.map
            (Set.filter (\(x, y) -> (x >= 0) && (y >= 0) && (x < width) && (y < height)))
            $ antinodes grid
}

nodesInRow :: String -> Map.Map Char (Set.Set Int)
nodesInRow line = Sequence.foldrWithIndex
    (\ i next acc ->
        if next == '.' then acc
        else Map.unionWith Set.union acc $ Map.singleton next $ Set.singleton i
    )
    Map.empty
    $ Sequence.fromList line

withNode :: Grid -> Char -> (Int, Int) -> Grid
withNode grid freq coord = grid {
    nodes = Map.insertWith Set.union freq (Set.singleton coord) $ nodes grid,
    antinodes = Map.insertWith Set.union freq (newAntinodes coord (Maybe.fromMaybe Set.empty (nodes grid Map.!? freq))) (antinodes grid)
}

newAntinodes :: (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
newAntinodes node nodes' = foldr Set.union Set.empty $
                                Set.map (newAntinodesSingle node) nodes'

newAntinodesSingle :: (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
newAntinodesSingle (x, y) (x', y') = let dx = x-x'; dy = y-y'; in Set.fromList [(x+dx, y+dy), (x'-dx, y'-dy)]