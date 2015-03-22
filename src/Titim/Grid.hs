module Titim.Grid
    ( Grid
    , Size
    , startGrid
    , makeStep
    ) where

import System.Random (randomRIO)
import Titim.Util (efficientNub)


-- The size represents the size in which letters
-- can be positioned on without destroying the grid.
type Size = (Int, Int)

-- The position is defined with regards
-- to a coordinate system that has 0 on the top
-- left corner of the grid, and x going right and
-- y going down.
type Position = (Int, Int)

-- An entity is just a letter and a position
-- in the grid.
type Entity = (Position, Char)

-- A grid is not destroyed if it can contain
-- all the letters. Once the letters spill out,
-- the grid is destroyed.
data Grid ent = Grid Size [ent] | Destroyed

instance Functor Grid where
    fmap fn (Grid size entities) =
        Grid size (fmap fn entities)
    fmap _ Destroyed = Destroyed

-- The grid starts with no entities..
startGrid :: Size -> Grid ent
startGrid size = Grid size []

-- ..which are filled at every step..
makeStep :: Grid ent -> IO (Grid ent)
makeStep = spawnLetters
         . moveLettersDown

-- ..and maybe destroyed.
hitWithWord :: String -> Grid ent -> Grid ent
hitWithWord word (Grid size entities) =
    Grid size (filter (not . (`elem` word) . snd) entities)

generatePosition :: Size -> IO Position
generatePosition (width, _) = do
    x <- randomRIO (0, width - 1)
    return (x, 0)

generatePositions :: Size -> IO [Position]
generatePositions (width, height) = do
    positions <- 
        forM [1..(width / 2)] 
            (const . generatePosition (width, height))
    return . efficientNub $ positions

generateLetter :: IO Char
generateLetter = do
    let alphabet = ['a'..'z']
    i <- randomRIO (0, length alphabet - 1)
    return $ alphabet !! i

spawnLetters :: Grid -> IO Grid
spawnLetters (Grid (width, height) entities) = do
    positions <- generatePositions
    newEntities <- mapM (\p -> do
        l <- generateLetter
        return (p, l))
    return $ Grid (width, height) (newEntities ++ entities)

-- At every step the letters are moved down.
moveLettersDown :: Grid -> Grid
moveLettersDown =
    let fn ((x, y), e) = ((x, y + 1), e) in
    fmap fn
