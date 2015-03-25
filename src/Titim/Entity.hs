module Titim.Entity where

import System.Random (randomRIO)

-- All the entities that can appear
-- on the game grid.
data Entity = Letter Char
            | Hit
            | Air 
            | House
            | Debris
            | Generator  deriving (Eq)

instance Show Entity where
    show (Letter c) = [c]
    show Hit = "."
    show Air = " "
    show House = "⌂"
    show Debris = "_"
    show Generator = "^"

-- This function generates in 2/3 cases a letter
-- of the internal alphabet.
tryGenLetter :: IO Entity
tryGenLetter = do
    p <- randomRIO (0, 2) :: IO Int
    if p > 0
        then do
            let alphabet = ['a'..'z']
            i <- randomRIO (0, length alphabet - 1)
            return $ Letter (alphabet !! i)
        else return Air

-- Hitting an entity with a word simply means
-- transforming letters that are in the word into hits.
hitEntity :: String -> Entity -> Entity
hitEntity word (Letter c) = 
    if c `elem` word
        then Hit
        else Letter c
hitEntity _ e = e

-- Describes how to calculate the score for a single
-- letter; it depends on the rarity of the letter.
-- @todo: Should depend on the rarity.
scoreForLetter :: Char -> Int
scoreForLetter _ = 10
