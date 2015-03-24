module Titim.IO where

import System.IO
import Text.Read (readMaybe)
import Titim.Grid (Size)

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
        [ "Titim 0.1"
        , "A game about falling letters"
        , ""
        , "titim :: Game"
        , "titim :: Width -> Height -> Game"
        ]

getSize :: (Int, Int) -> [String] -> (Int, Int)
getSize def [widthStr, heightStr] =
    case (readMaybe widthStr, readMaybe heightStr) of
        (Just width, Just height) -> (width, height)
        _                         -> def
getSize def _ = def
