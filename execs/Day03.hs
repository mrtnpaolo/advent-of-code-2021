module Main (main) where

import Advent

import Numeric   (readBin)
import Data.List (transpose)

main =
  do inp <- getInputLines id 3
     print (part1 inp)
     print (part2 inp)

part1 xs = gamma*eps
  where
    gammaS = map mostCommon . transpose $ xs
    [(gamma,_)] = readBin gammaS

    epsS = map leastCommon . transpose $ xs
    [(eps,_)] = readBin epsS

mostCommon :: String -> Char
mostCommon xs
  | count ('0'==) xs > count ('1'==) xs = '0'
  | otherwise                           = '1'

leastCommon xs
  | count ('0'==) xs > count ('1'==) xs = '1'
  | otherwise                           = '0'

part2 xs = oxy * co2
  where
    go _ _ [xs] = xs
    go p n xs = go p (n+1) xs'
      where
        ns = head . drop n . transpose $ xs
        digit = p ns
        xs' = filter ((digit ==) . head . drop n) xs

    [(oxy,_)] = readBin $ go mostCommon 0 xs
    [(co2,_)] = readBin $ go leastCommon 0 xs
