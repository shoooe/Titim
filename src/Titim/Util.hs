module Titim.Util where

import qualified Data.Set as Set

efficientNub :: (Eq a, Ord a) => [a] -> [a]
efficientNub = Set.toAscList . Set.fromList
