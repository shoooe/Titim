module Main where

import Titim.Game
import Titim.Util

main :: IO ()
main = do
    game <- startGame (10, 10)
    finalGame <- buildUntil isGameOver gameStep game
    gameOverScreen finalGame
