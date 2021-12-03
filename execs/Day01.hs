module Main (main) where

import Advent (getInputLines,count)

main =
  do xs <- getInputLines parse 1
     print (part1 xs)
     print (part2 xs)
  where
    parse = read @Int

solve n xs = count (uncurry (<)) (zip xs (drop n xs))

part1 = solve 1

part2 = solve 3
