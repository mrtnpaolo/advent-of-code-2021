module Main (main) where

import Advent     (getInputLines,count)
import Data.Ord   (comparing)
import Data.List  (foldl',sort,sortBy,splitAt,(\\),elemIndex)
import Data.Maybe (fromJust)

main =
  do inp <- getInputLines parse 8
     print (part1 inp)
     print (part2 inp)
  where
    parse = words . map sep
      where
        sep '|' = ' '
        sep  x  =  x

part1 = count simple . concatMap (drop 10)

simple xs = length xs `elem` [2,3,4,7]

part2 = sum . map solve

solve xs = foldl' (\x y -> x*10 + y) 0 digits
  where
    (ins,outs) = splitAt 10 xs

    digits = decode (unscramble ins) outs

unscramble ins = [l0,l1,l2,l3,l4,l5,l6,l7,l8,l9]
  where
    [l1,l7,l4,s5a,s5b,s5c,s6a,s6b,s6c,l8] = sortBy (comparing length) ins

    neg = (l8 \\)

    s5 = [s5a,s5b,s5c] -- 5-segment digits

    [l3] = [ x | x <- s5, length (x \\ l1)     == 3 ]
    [l2] = [ x | x <- s5, length (x \\ neg l4) == 2 ]
    [l5] = s5 \\ [l3,l2]

    s6 = [s6a,s6b,s6c] -- 6-segment digits

    [l0] = [ x | x <- s6, length (x \\ l5) == 2 ]
    [l9] = [ x | x <- s6, length (x \\ l4) == 2 ]
    [l6] = s6 \\ [l0,l9]


decode key outs = map digit outs
  where
    digit x = fromJust $ elemIndex (sort x) (map sort key)
