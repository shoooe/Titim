module Titim.Util where

-- Kind of similar to a fold, it keeps updating
-- the given accumulator, until a certain condition is met.
buildUntil :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
buildUntil predicate action ini = do
    ini' <- action ini
    if predicate ini'
        then return ini'
        else buildUntil predicate action ini'

-- Performs an action and then checks its validity:
-- if it's valid it returns the value, otherwise it repeats the action.
repeatUntil :: (Monad m) => (a -> m Bool) -> m a -> m a
repeatUntil predicate action = do
    x <- action
    b <- predicate x
    if b then return x else repeatUntil predicate action

-- This intersperses an element every n 
-- elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf 0 _ = []
chunksOf _ [] = []
chunksOf n ls = 
    let (x, r) = splitAt n ls
    in  x : chunksOf n r
