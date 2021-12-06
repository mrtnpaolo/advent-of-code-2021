module Main (main) where

import Advent (getInput,count)

main =
  do inp <- getInput parse 6
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = read @[Int] $ '[':xs++"]"

timers xs = [ fromIntegral (count (i==) xs) | i <- [0..8] ] :: [Integer]

part1 fish = sum $ zipWith (*) (timers fish) [1421,1401,1191,1154,1034,950,905,779,768]

part2 fish = sum $ zipWith (*) (timers fish) [6703087164,6206821033,5617089148
                                             ,5217223242,4726100874,4368232009
                                             ,3989468462,3649885552,3369186778]
