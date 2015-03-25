module Titim.Game where

import           Titim.Grid
import           Data.Set (Set)
import qualified Data.Set as Set

-- Data structure that represents the entire
-- game being played.
data Game = 
    Game 
        { getGameScore :: Int
        , getGameGrid :: Grid
        , getGameWords :: Set String
        , getGameUsedWords :: Set String
        }

instance Show Game where
    show (Game score grid _ _) =
        concat 
            ["Score: ", show score, "\n\n"
            , show grid
            ]

-- Represents a word validation error for the game.
data ValidationError = AlreadyUsed | NotAWord deriving (Eq)

-- Generates a game with the given dictionary and
-- size of the grid.
makeGame :: FilePath -> (Int, Int) -> IO Game
makeGame dict size = do
    dictionary <- readFile dict
    let wordz = Set.fromList . lines $ dictionary
    let usedWords = Set.empty
    return $ Game 0 (makeGrid size) wordz usedWords

-- Checks if there are more debris than houses.
isGameOver :: Game -> Bool
isGameOver (Game _ grid _ _) = 
    countEntity Debris grid > countEntity House grid

-- Hits the game with the given word, updating the 
-- corresponding sets and the grid itself.
hitGame :: String -> Game -> Game
hitGame word (Game score grid ws uws) =
    let (s, grid')  = hitGrid word grid
        score'      = score + s
        ws'         = Set.delete word ws
        uws'        = Set.insert word uws
    in  Game score' grid' ws' uws'

-- Updating a game just forwards the update to
-- the grid.
updateGame :: Game -> IO Game
updateGame (Game score grid ws uws) = do
    grid' <- updateGrid grid
    return $ Game score grid' ws uws

-- Checks if a word can be used and if it can't
-- it returns a proper error message.
validateWord :: String -> Game -> Either ValidationError String
validateWord [] _ = Right []
validateWord word (Game _ _ ws uws) =
    case (Set.member word ws, Set.member word uws) of
        (True, _) -> Right word
        (False, True) -> Left AlreadyUsed
        (False, False) -> Left NotAWord
