module Titim.IO where

import System.IO
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Titim.Game
import Titim.Util

-- Clears the output screen, by removing all lines
-- and setting the cursor to the top left corner.
clearScreen :: IO ()
clearScreen = putStr "\o33c"

-- Simply puts a lable on the screen.
askLabel :: String -> IO ()
askLabel label = do
    putStr label
    hFlush stdout

-- Goes back one line and ask with a label
-- again.
askLabelOver :: String -> IO ()
askLabelOver str = do
    putStr $ "\o33[1A\r\o33[K" ++ str
    hFlush stdout

-- Utility used to show a screen of text.
showScreen :: [String] -> IO ()
showScreen ls= do
    clearScreen
    putStrLn "\n--------------"
    putStr $ unlines ls
    putStrLn "--------------\n"

-- Screen shown at the beginning of the game.
splashScreen :: (Int, Int) -> IO ()
splashScreen _ =
    showScreen
        [ "A bunch of letters are falling from the sky, "
        , "threatening the village below."
        , "Your mission is to create words by stealing as many "
        , "letters as possible."
        , ""
        , "Press enter to begin..."
        ]

-- Help screen, shown when the user does something
-- silly.
helpScreen :: IO ()
helpScreen =
    showScreen
        [ "Titim"
        , "Usage: titim ROWS COLUMNS"
        , ""
        , "The score bonus is inversely proportional to "
        , "the height of the grid, and directly proportional to "
        , "its width."
        ]

-- Parses the size out of a bunch of strings.
getSize :: [String] -> Maybe (Int, Int)
getSize [rowsStr, colsStr] =
    case (readMaybe rowsStr, readMaybe colsStr) of
        (Just rows, Just cols) -> 
            if rows > 0 && cols > 0 -- counting the house
                then Just (rows + 2, cols) -- we add 1 for the houses and generators
                else Nothing
        _ -> Nothing
getSize _ = Nothing

-- Asks for a valid size, returns `Nothing`
-- if the size could not be parsed.
askSize :: IO (Maybe (Int, Int))
askSize = liftM getSize getArgs

-- Asks for a word until it's valid for the game.
askWord :: Game -> IO String
askWord game = repeatUntil action getLine
    where action word =
              case validateWord word game of
                  Left err -> do
                      askLabelOver $ errorMessage err ++ ", give me another: "
                      return False
                  Right _ -> return True
          errorMessage AlreadyUsed = "Already used"
          errorMessage NotAWord = "Not even a word"

-- Screen showns when the game is over and the player
-- effectively lost.
gameOverScreen :: Game -> IO ()
gameOverScreen game =
    let score = getGameScore game
        nwords = countUsedWords game
    in showScreen
        [ "Too many houses were destroyed!"
        , "Score: " ++ show score ++ " points"
        , "Used: " ++ show nwords ++ " words"
        ]

-- Procedure to perform at each step of the game.
gameStep :: Game -> IO Game
gameStep game = do
    clearScreen
    print game
    askLabel "Give me a word: "
    word <- askWord game
    updateGame . hitGame word $ game

-- Repeats the steps until the game is in a game over
-- status.
gameLoop :: Game -> IO Game
gameLoop = buildUntil isGameOver gameStep

-- Initializes the game and runs it.
runGame :: (Int, Int) -> FilePath -> IO ()
runGame size dict = do
    splashScreen size
    _ <- getLine
    game <- makeGame dict size
    finalGame <- gameLoop game 
    gameOverScreen finalGame
