module Main where

import Titim.Game
import Titim.Util
import Titim.IO

main :: IO ()
main = do
    maybeSize <- askSize
    case maybeSize of
        Nothing -> helpScreen
        Just size -> do
            splashScreen size
            _ <- getLine
            game <- startGame "/usr/share/dict/words" size
            finalGame <- buildUntil isGameOver gameStep game
            gameOverScreen finalGame
