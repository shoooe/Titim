{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Titim.Interpreter (interpret) where

import Control.Applicative
import System.Console.CmdTheLine
import Titim.IO (runGame)

type Size = (Int, Int)

instance ArgVal Size where
    converter = pair ','

instance ArgVal (Maybe Size) where
    converter = just

sizeArg :: Term Size 
sizeArg =
    value . opt (7, 33) $
        (optInfo ["g", "grid"])
            { optName = "GRID"
            , optDoc = "gridsize defined as `rows,columns`"
            }

dictArg :: Term String
dictArg =
    fileExists . value . opt "/usr/share/dict/words" $
        (optInfo ["d", "dictionary"])
            { optName = "DICTIONARY"
            , optDoc = "path to the dictionary file"
            }

playTerm :: (Term (IO ()), TermInfo)
playTerm = 
    ( runGame
        <$> sizeArg
        <*> dictArg
    , defTI 
        { termName = "titim"
        , termDoc = "Plays the game with the defined settings"
        } )

interpret :: IO () 
interpret = run playTerm
