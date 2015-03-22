module Titim.Util where

import Control.Monad

buildUntil :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
buildUntil predicate action init = do
    init' <- action init
    if predicate init'
        then return init'
        else buildUntil predicate action init'
