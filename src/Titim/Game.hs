module Titim.Game where

import Data.Set (Set)
import qualified Data.Set as Set

import Titim.Grid
import Titim.IO

data Game = 
    Game 
        { getScore :: Int
        , getGrid :: Grid
        , getWords :: Set String
        , getUsedWords :: Set String
        }

instance Show Game where
    show (Game score grid _ _) =
        concat 
            ["Score: ", show score, "\n\n"
            , show grid
            ]

startGame :: FilePath -> Size -> IO Game
startGame dict (w, h) = do
    dictionary <- readFile dict
    let wordz = Set.fromList . lines $ dictionary
    let usedWords = Set.empty
    return $ Game 0 (startGrid (w, h)) wordz usedWords

askWord :: Set String -> Set String -> IO String
askWord wordz usedWords = 
    askUntil (\word ->
        case (word `Set.member` wordz, word `Set.member` usedWords) of
            (True, _)       -> return True
            (False, True)   -> do
                askLabelOver "Used already, give me another: "
                return False
            (False, False) -> do
                askLabelOver "Not a word, give me another: "
                return False) getLine

isGameOver :: Game -> Bool
isGameOver (Game _ grid _ _) = manyDed grid

gameOverScreen :: Game -> IO ()
gameOverScreen (Game score _ _ _) =
    showScreen
        [ "More than half of the houses have been destroyed!"
        , "You failed your mission with a score of " ++ show score ++ " points."
        , ":c"
        ]

gameStep :: Game -> IO Game
gameStep game@(Game score grid wordz usedWords) = do
    clearScreen
    print $ game
    askLabel "Give me a word: "
    word <- askWord wordz usedWords
    let (currentScore, grid') = hitWithWord word grid
    let score' = score + currentScore
    grid'' <- makeStep grid'
    return $ 
        Game score' grid''
            (Set.delete word wordz) 
            (Set.insert word usedWords)
