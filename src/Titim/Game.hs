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

startGame :: Size -> IO Game
startGame size = do
    dictionary <- readFile "dictionary.txt"
    let words = Set.fromList . lines $ dictionary
    let usedWords = Set.empty
    return $ Game 0 (startGrid size) words usedWords

askWord :: Set String -> Set String -> IO String
askWord words usedWords = 
    askUntil (\word ->
        case (word `Set.member` words, word `Set.member` usedWords) of
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
gameOverScreen game@(Game score _ _ _) =
    showScreen
        [ "More than half of the houses have been destroyed!"
        , "You failed your mission with a score of " ++ show score ++ " points."
        , ":c"
        ]

scoreFor :: Grid -> Int
scoreFor = sum . map (\_ -> 10) . getHits

gameStep :: Game -> IO Game
gameStep game@(Game score grid words usedWords) = do
    clearScreen
    grid' <- makeStep grid
    print $ game { getGrid = grid' }
    askLabel "Give me a word: "
    word <- askWord words usedWords
    let grid'' = hitWithWord word grid'
    let score' = score + scoreFor grid''
    return $ 
        Game score' grid''
            (Set.delete word words) 
            (Set.insert word usedWords)
