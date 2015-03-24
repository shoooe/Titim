module Main where

import Titim.Game
import Titim.Util
import Titim.IO
import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
    size <- liftM (getSize (27, 7)) getArgs 
    splashScreen size
    _ <- getLine
    game <- startGame size
    finalGame <- buildUntil isGameOver gameStep game
    gameOverScreen finalGame
