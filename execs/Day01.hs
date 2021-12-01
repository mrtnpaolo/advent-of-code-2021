module Main (main) where

import Advent

import Data.List (unfoldr)

main =
  do xs <- getInputLines parse 1
     print (part1 xs)
     print (part2 xs)
  where
    parse = read @Int

part1 :: [Int] -> Int
part1 xs = count (>0) changes
  where
    changes = zipWith (-) (tail xs) xs

part2 :: [Int] -> Int
part2 xs = count (>0) changes
  where
    sums = map sum (unfoldr (window 3) xs)

    window n xs
      | length xs >= n = Just (take n xs,tail xs)
      | otherwise      = Nothing

    changes = zipWith (-) (tail sums) sums
