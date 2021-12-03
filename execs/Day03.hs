module Main (main) where

import Advent

import Numeric   (readBin)
import Data.List (transpose)

main =
  do inp <- getInput lines 3
     print (part1 inp)
     print (part2 inp)

part1 xs = γ*ε
  where
    γ = toDecimal . map most  . transpose $ xs
    ε = toDecimal . map least . transpose $ xs

most xs
  | count ('0'==) xs > count ('1'==) xs = '0'
  | otherwise                           = '1'

least xs
  | most xs == '0' = '1'
  | otherwise      = '0'

part2 xs = o₂ * co₂
  where
    o₂  = sieve 0 most  xs
    co₂ = sieve 0 least xs

    sieve _ _ [x] = toDecimal x
    sieve n p xs  = sieve (n+1) p xs'
      where
        digit = p (transpose xs !! n)
        xs'   = filter (\x -> digit == x !! n) xs

toDecimal xs = let [(n,_)] = readBin xs in n
