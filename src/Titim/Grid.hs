module Titim.Grid
    ( Grid
    , Size
    , startGrid
    , makeStep
    , hitWithWord
    , manyDed
    ) where

import System.Random (randomRIO)
import Data.Vector (Vector, (!?), (//), (!))
import qualified Data.Vector as V

type Size = (Int, Int)
type Position = (Int, Int)

toPosition :: Size -> Int -> Position
toPosition (w, _) i = (i `mod` w, i `div` w)

toIndex :: Size -> Position -> Int
toIndex (w, _) (x, y) = y * w + x

onTop :: Position -> Position
onTop (x, y) = (x, y - 1)

data Entity = Letter Char | Air | ToSave | NotSaved | Hit Char deriving (Eq)
data Grid = Grid Size (Vector Entity)

charFor :: Entity -> Char
charFor (Letter c) = c
charFor Air = ' '
charFor ToSave = '⌂'
charFor NotSaved = '_'
charFor (Hit _) = '.'

instance Show Grid where
    show (Grid (w, _) entities) =
        concat (replicate w " _") ++ "\n" ++
        V.ifoldr 
            (\i e str ->
                if (((i + 1) `mod` w) == 0)
                    then ' ' : charFor e : '\n' : str
                    else ' ' : charFor e : str)
            [] entities

lastRowIndices :: Size -> [Int]
lastRowIndices (w, h) = [(w * (h - 1))..(w * h - 1)]

startGrid :: Size -> Grid
startGrid size@(w, h) = 
    let emptyMatrix = V.replicate (w * h) Air
        updates = map (\i -> (i, ToSave)) (lastRowIndices size) in
    Grid size (emptyMatrix // updates)

makeStep :: Grid -> IO Grid
makeStep = spawnLetters . moveDown

hitEntityWithWord :: String -> Entity -> Entity
hitEntityWithWord word (Letter c) = 
    if c `elem` word
        then Hit c
        else Letter c
hitEntityWithWord _ e = e

letterValue :: Char -> Int
letterValue _ = 10
    
normalizeScore :: Size -> Int -> Int
normalizeScore (w, h) i = (i * w) `div` h

addScoreFor :: String -> Entity -> Int -> Int
addScoreFor word (Letter c) =
    if c `elem` word
        then (+ letterValue c)
        else id
addScoreFor _ _ = id

hitWithWord :: String -> Grid -> (Int, Grid)
hitWithWord word (Grid size entities) =
    let entities' = V.map (hitEntityWithWord word) entities
        score = V.foldr (addScoreFor word) 0 entities in
    (normalizeScore size score, Grid size entities')

numDed :: Grid -> Int
numDed (Grid size entities) =
    foldl 
        (\b i -> 
            if (entities ! i) == NotSaved 
                then b + 1
                else b)
        0 (lastRowIndices size)

manyDed :: Grid -> Bool
manyDed grid@(Grid (w, _) _) =
    numDed grid > w `div` 2

getCell :: Grid -> Position -> Entity
getCell (Grid size entities) pos =
    case entities !? (toIndex size pos) of
        Nothing -> Air
        Just x -> x

moveDown :: Grid -> Grid
moveDown grid@(Grid size entities) =
    let stealFromTop :: Grid -> Int -> Entity -> Entity
        stealFromTop g i e = 
            let topCell = getCell g (onTop (toPosition size i)) in
            case (topCell, e) of
                (Letter _, ToSave)  -> NotSaved
                (_, ToSave)         -> ToSave
                (_, NotSaved)       -> NotSaved
                _                   -> topCell
        entities' = V.imap (stealFromTop grid) entities in
    Grid size entities'

getRandomLetter :: IO Entity
getRandomLetter = do
    let alphabet = ['a'..'z']
    i <- randomRIO (0, length alphabet - 1)
    return $ Letter (alphabet !! i)

maybeSpawn :: IO Entity
maybeSpawn = do
    x <- (randomRIO (0, 2) :: IO Int)
    if x > 0 
        then getRandomLetter
        else return Air

spawnLetters :: Grid -> IO Grid
spawnLetters (Grid (width, height) entities) = do
    let indices = V.enumFromN 0 width
    updates <- V.mapM (\i -> maybeSpawn >>= return . ((,) i)) indices
    return $ Grid (width, height) (V.update entities updates)
