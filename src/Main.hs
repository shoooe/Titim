module Main where

import Titim.Grid
import Control.Monad (foldM_)
import Data.Set (Set)
import qualified Data.Set as Set

data Game = Game Grid (Set String) (Set String)

main :: IO ()
main = do
    dictionary <- readFile "dictionary.txt"
    initial <- makeStep $ startGrid (10, 10)
    foldM_ 
        (\(Game grid words usedWords) i -> do
            putStr "\o33c"
            putStrLn $ "Round " ++ show i
            putStrLn "----------------------------"
            print grid
            putStrLn "----------------------------"
            putStrLn "Give me a word: "
            word <- getLine
            if word `Set.member` words
                then do
                    grid' <- makeStep $ hitWithWord word grid
                    return $ Game grid' (Set.delete word words) (Set.insert word usedWords)
                else do
                    if word `Set.member` usedWords
                        then putStrLn "You used that one already"
                        else putStrLn "That's not a word you fool"
                    putStrLn "Say something bad to me: "
                    _ <- getLine
                    grid' <- makeStep grid
                    return $ Game grid' words usedWords)
        (Game initial (Set.fromList $ lines dictionary) Set.empty)
        [1..]
