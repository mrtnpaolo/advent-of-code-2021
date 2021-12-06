module Main (main) where

import Advent (getInput,freqs)
import Data.IntMap.Strict qualified as IM

main =
  do inp <- getInput parse 6
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = read @[Int] $ '[':xs++"]"

part1 = day 80

part2 = day 256

day n fish = total (iterate spawn (freqs fish) !! n)

spawn = combine . concatMap tick
  where
    tick (0,n) = [(6,n),(8,n)]
    tick (i,n) = [(i-1,n)]

    combine = IM.toAscList . IM.fromListWith (+)

total xs = sum [ n | (_,n) <- xs ]

