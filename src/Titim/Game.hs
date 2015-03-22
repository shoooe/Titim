module Titim.Game where

import Data.Set (Set)
import qualified Data.Set as Set

import Titim.Grid
import Titim.IO

data Game = Game Int Grid (Set String) (Set String)

instance Show Game where
    show (Game score grid _ _) =
        concat 
            ["Score: ", show score, "\n\n"
            , show grid
            ]

startGame :: Size -> IO Game
startGame size = do
    initialGrid <- makeStep $ startGrid size
    dictionary <- readFile "dictionary.txt"
    let words = Set.fromList . lines $ dictionary
    let usedWords = Set.empty
    return $ Game 0 initialGrid words usedWords

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
isGameOver (Game _ grid _ _) = allDed grid

gameOverScreen :: Game -> IO ()
gameOverScreen game@(Game score _ _ _) = do
    clearScreen
    putStrLn "\n--------------"
    putStrLn "Game over!"
    putStrLn $ "Your score: " ++ show score
    putStrLn "--------------\n"

gameStep :: Game -> IO Game
gameStep game@(Game score grid words usedWords) = do
    clearScreen
    print game
    askLabel "Give me a word: "
    word <- askWord words usedWords
    grid' <- makeStep $ hitWithWord word grid
    return $ 
        Game score grid' 
            (Set.delete word words) 
            (Set.insert word usedWords)
