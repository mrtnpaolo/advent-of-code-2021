module Advent.List
  ( count
  ) where

import Data.List (foldl')

count :: (a -> Bool) -> [a] -> Int
count p xs = foldl' f 0 xs
  where
    f n x | p x = n+1 | otherwise = n
