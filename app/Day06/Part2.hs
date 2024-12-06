module Day06.Part2 where

import Data.List
import Data.Maybe

run :: String -> Int
run contents =
    let
        state = parseGameState contents
        barrelPositions = uniquePositions $ fromJust (playGame state)
        statesWithObstacles = map (\(x, y) -> state { obstacles = (x, y):obstacles state }) barrelPositions
    in
        length $ filter (isNothing . playGame) statesWithObstacles

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

-- returns none if bad
playGame :: GameState -> Maybe [GameState]
playGame initialState = reverse <$> playGame' [initialState]

playGame' :: [GameState] -> Maybe [GameState]
playGame' prevStates =
    let state' = nextState (head prevStates)
    in
        if isGameOver state' then Just prevStates
        else if getGuard state' `elem` map getGuard prevStates then Nothing
        else playGame' $ state':prevStates

data Direction = N | E | S | W deriving (Eq)
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
getGuard :: GameState -> (Direction, Int, Int)
getGuard (GameState _ _ dir (x, y)) = (dir, x, y)

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

uniquePositions :: [GameState] -> [(Int, Int)]
uniquePositions = nub . map guardPos
