module Titim.Util where

buildUntil :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
buildUntil predicate action ini = do
    ini' <- action ini
    if predicate ini'
        then return ini'
        else buildUntil predicate action ini'
