module Day06.Part1 where

import Data.List
import Data.Maybe

run :: String -> Int
run = countUniquePositions . playGame . parseGameState

parseGameState :: String -> GameState
parseGameState contents =
    let
        rows = lines contents
        hashIndices = indexed $ map (elemIndices '#') rows
        hashPositions = (\(y, indices) -> zip indices (replicate (length indices) y)) =<< hashIndices
        guards = indexed $ map guardInRow rows
        guard' = fromJust $ find (\(_, p) -> isJust p) guards -- assume exactly one guard
        gdir = fst (fromJust $ snd guard')
        gpos = (snd (fromJust $ snd guard'), fst guard')
    in
        GameState {
            dimensions = (length rows, length $ head rows),
            obstacles = hashPositions,
            guardDir = gdir,
            guardPos = gpos
        }

guardInRow :: String -> Maybe (Direction, Int)
guardInRow line =
    let
        idx = findIndex (`elem` "^>v<") line
        char = fmap (line !!) idx
    in
        case (fromChar =<< char, idx) of
            (Just dir, Just i) -> Just (dir, i)
            _ -> Nothing

playGame :: GameState -> [GameState]
playGame initialState = reverse $ playGame' [initialState]

playGame' :: [GameState] -> [GameState]
playGame' prevStates =
    let state' = nextState (head prevStates)
    in  if isGameOver state' then prevStates else playGame' $ state':prevStates

countUniquePositions :: [GameState] -> Int
countUniquePositions = length . nub . map guardPos

data Direction = N | E | S | W
instance Show Direction where
    show N = "^"
    show E = ">"
    show S = "v"
    show W = "<"

nextDir :: Direction -> Direction
nextDir N = E
nextDir E = S
nextDir S = W
nextDir W = N

fromChar :: Char -> Maybe Direction
fromChar '^' = Just N
fromChar '>' = Just E
fromChar 'v' = Just S
fromChar '<' = Just W
fromChar _ = Nothing

nextPos :: Direction -> (Int, Int) -> (Int, Int)
nextPos N (x, y) = (x, y-1)
nextPos E (x, y) = (x+1, y)
nextPos S (x, y) = (x, y+1)
nextPos W (x, y) = (x-1, y)

data GameState = GameState {
    dimensions :: (Int, Int),
    obstacles :: [(Int, Int)],
    guardDir :: Direction,
    guardPos :: (Int, Int)
}
instance Show GameState where
    show (GameState _ _ gd gp) = show gd ++ show gp

isGameOver :: GameState -> Bool
isGameOver state =
    let
        (x, y) = guardPos state
        (w, h) = dimensions state
    in
        (x == -1 || y == -1 || x == w || y == h)

nextState :: GameState -> GameState
nextState state =
    let
        contStraight = nextPos (guardDir state) (guardPos state)
    in
        if contStraight `elem` obstacles state then state { guardDir = nextDir $ guardDir state }
        else state { guardPos = contStraight }

indexed :: [a] -> [(Int, a)]
indexed = indexed' 0

indexed' :: Int -> [a] -> [(Int, a)]
indexed' _ [] = []
indexed' count list = (count, head list):indexed' (count+1) (tail list)
