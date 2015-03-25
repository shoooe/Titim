module Titim.Grid
    ( Grid
    , makeGrid
    , updateGrid
    , hitGrid
    , countEntity
    , module Titim.Entity
    ) where

import           Data.Matrix (Matrix)
import           Titim.Entity
import           Data.List (intercalate)
import           Data.Foldable (foldr)
import           Data.Traversable (mapM)
import           Prelude hiding (foldr, mapM)
import qualified Data.Matrix as Mat

-- This is a new type so that we can
-- define an instance of `Show`.
newtype Grid = Grid (Matrix Entity)

instance Show Grid where
    show (Grid matrix) =
        unlines
            . map (intercalate " ")
            . map (map show)
            . Mat.toLists $ matrix

-- Starts the grid with a row of generators on top
-- and a row of houses on the bottom.
makeGrid :: (Int, Int) -> Grid
makeGrid (rows, cols) =
    Grid $ Mat.matrix rows cols gen
    where gen (i, _)
              | i == 1    = Generator
              | i == rows = House
              | otherwise = Air

-- Makes 1 logic step for the grid.
updateGrid :: Grid -> IO Grid
updateGrid grid@(Grid matrix) = do
    matrix' <- imapM (moveDownCell grid) matrix
    return $ Grid matrix'

-- Hits the entire grid with the word, transforming
-- letters into hits and returning the total score for the hit.
hitGrid :: String -> Grid -> (Int, Grid)
hitGrid word (Grid matrix) =
    let matrix' = fmap (hitEntity word) matrix
        score   = foldr (addScoreFor word) 0 matrix
        size    = (Mat.nrows matrix, Mat.ncols matrix)
    in  (normalizeScore size score, Grid matrix')

-- Counts the number of entities in the grid that are
-- equal to the one given.
countEntity :: Entity -> Grid -> Int
countEntity s (Grid matrix) =
    foldr (\e -> if e == s then (+ 1) else id) 0 matrix

-- Takes a word and an entity and a counter and returns
-- the new counter based on the hitting of the letter.
addScoreFor :: String -> Entity -> Int -> Int
addScoreFor word (Letter c) =
    if c `elem` word
        then (+ scoreForLetter c)
        else id
addScoreFor _ _ = id

-- The score depends on the size of the grid.
normalizeScore :: (Int, Int) -> Int -> Int
normalizeScore (r, c) i = (i * c) `div` r

-- Performs a row major map with index.
imap :: ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
imap fn matrix =
    let (r, c) = (Mat.nrows matrix, Mat.ncols matrix)
        perElem (y, x) = fn (y, x) (Mat.getElem y x matrix)
    in  fmap perElem $ Mat.matrix r c id

-- Performs a row major map with index over a
-- monadic context.
imapM :: (Monad m) => ((Int, Int) -> a -> m b) -> Matrix a -> m (Matrix b)
imapM fn matrix =
    let (r, c) = (Mat.nrows matrix, Mat.ncols matrix)
        perElem (y, x) = fn (y, x) (Mat.getElem y x matrix)
    in  mapM perElem $ Mat.matrix r c id

-- Gets the element if within the bounds of the matrix
-- otherwise it returns `Air`.
elemGet :: Grid -> (Int, Int) -> Entity
elemGet (Grid matrix) (r, c) =
    case Mat.safeGet r c matrix of
        Nothing -> Air
        Just x  -> x

-- Performs the move down logic on a single
-- cell of the grid.
moveDownCell :: Grid -> (Int, Int) -> Entity -> IO Entity
moveDownCell grid (r, c) e =
    let northElem = elemGet grid (r - 1, c) in
    case (northElem, e) of
        (Letter _, House)   -> return Debris
        (_, House)          -> return House
        (_, Generator)      -> return Generator
        (_, Debris)         -> return Debris
        (Generator, _)      -> tryGenLetter
        _                   -> return northElem
