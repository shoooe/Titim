module Main where

import Titim.Game
import Titim.Util
import Titim.IO
import System.Environment (getArgs)

splashScreen :: IO ()
splashScreen =
    showScreen
        [ "A bunch of letters are falling from the sky, "
        , "threatening the village below."
        , "Your mission is to create words by stealing as many "
        , "words as possible."
        , ""
        , "Press enter to begin..."
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [width, height] -> do
            splashScreen 
            _ <- getLine
            game <- startGame (read width, read height)
            finalGame <- buildUntil isGameOver gameStep game
            gameOverScreen finalGame
        _ -> putStrLn "titim :: Width -> Height -> Game"
