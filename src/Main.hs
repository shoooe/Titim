module Main where

import Titim.Grid
import Control.Monad (foldM_)

main :: IO ()
main = do
    initial <- makeStep $ startGrid (10, 10)
    foldM_ 
        (\grid _ -> do
            putStr "\o33c"
            putStrLn "----------------------------"
            print grid
            putStrLn "----------------------------"
            putStrLn "Your answer: "
            word <- getLine
            grid' <- makeStep grid
            return . hitWithWord word $ grid') 
        initial
        [1..]
