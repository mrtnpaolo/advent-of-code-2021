module Main (main) where

import Advent     (getInputLines,count)
import Data.Ord   (comparing)
import Data.List  (foldl',sort,sortBy,splitAt,(\\),elemIndex)

main =
  do inp <- getInputLines parse 8
     print (part1 inp)
     print (part2 inp)
  where
    parse = map sort . words . map sep
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

    x `minus` y = length (x \\ y)

    s5 = [s5a,s5b,s5c] -- 5-segment digits

    [l2] = [ x | x <- s5           , x `minus` l4 == 3 ]
    [l3] = [ x | x <- s5 \\ [l2]   , x `minus` l7 == 2 ]
    [l5] = [ x | x <- s5 \\ [l2,l3]                    ]

    s6 = [s6a,s6b,s6c] -- 6-segment digits

    [l9] = [ x | x <- s6           , x `minus` l4 == 2 ]
    [l0] = [ x | x <- s6 \\ [l9]   , x `minus` l7 == 3 ]
    [l6] = [ x | x <- s6 \\ [l9,l0]                    ]

decode key outs = map digit outs
  where
    digit x = let Just d = elemIndex x key in d
