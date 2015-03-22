module Titim.IO where

import System.IO

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
