module Titim.IO where

import System.IO
import Text.Read (readMaybe)
import Titim.Grid (Size)
import System.Environment (getArgs)
import Control.Monad (liftM)

askUntil :: (Monad m) => (a -> m Bool) -> m a -> m a
askUntil predicate action = do
    x <- action
    b <- predicate x
    if b then return x else askUntil predicate action

clearScreen :: IO ()
clearScreen = putStr "\o33c"

askLabel :: String -> IO ()
askLabel label = do
    putStr label
    hFlush stdout

askLabelOver :: String -> IO ()
askLabelOver str = do
    putStr $ "\o33[1A\r\o33[K" ++ str
    hFlush stdout

showScreen :: [String] -> IO ()
showScreen ls= do
    clearScreen
    putStrLn "\n--------------"
    putStr $ unlines ls
    putStrLn "--------------\n"

splashScreen :: Size -> IO ()
splashScreen (width, height) =
    showScreen
        [ "A bunch of letters are falling from a "
            ++ show width ++ "x" ++ show height ++ " sky, "
        , "threatening the village below."
        , "Your mission is to create words by stealing as many "
        , "letters as possible."
        , ""
        , "Press enter to begin..."
        ]

helpScreen :: IO ()
helpScreen =
    showScreen
        [ "Titim"
        , "Usage: titim WIDTH HEIGHT"
        , ""
        , "The score bonus is inversely proportional to "
        , "the height of the grid, and directly proportional to "
        , "its width."
        ]

getSize :: [String] -> Maybe (Int, Int)
getSize [widthStr, heightStr] =
    case (readMaybe widthStr, readMaybe heightStr) of
        (Just width, Just height) -> 
            if width > 0 && height > 0 -- counting the house
                then Just (width, height + 1) -- we had 1 for the houses
                else Nothing
        _ -> Nothing
getSize _ = Nothing

askSize :: IO (Maybe (Int, Int))
askSize = liftM getSize getArgs 
