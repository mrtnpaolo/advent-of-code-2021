module Advent.List
  ( count
  , freqs
  ) where

import Data.List (foldl')
import Data.Map.Strict qualified as M (toAscList,fromListWith)

count :: (a -> Bool) -> [a] -> Int
count p xs = foldl' f 0 xs
  where
    f n x | p x = n+1 | otherwise = n

freqs :: (Ord a) => [a] -> [(a,Int)]
freqs xs = combine [ (x,1) | x <- xs ]
  where
    combine = M.toAscList . M.fromListWith (+)
