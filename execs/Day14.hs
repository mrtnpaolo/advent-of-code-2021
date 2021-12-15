module Main (main) where

import Advent             (getInputLines)
import Data.Map.Strict    qualified as M
import Data.Array.Unboxed qualified as A

main =
  do inp <- unpack <$> getInputLines parse 14
     print (part1 inp)
     print (part2 inp)
  where
    parse = words . map \case c | c `elem` "->" -> ' '; c -> c
    unpack ([x]:_:xs) = (x,toA xs)
    toA xs = empty A.// rules :: A
      where
        rules = map (\[[a,b],[c]] -> ((a,b),c)) xs
        empty = A.listArray (('A','A'),('Z','Z')) (repeat '\0')

type A = A.UArray (Char,Char) Char

polymerize :: String -> A -> Int -> Int
polymerize base rules n = summarize (iterate step start !! n)
  where
    start = M.fromListWith (+) [ (c,1) | c <- zip base (tail base) ]

    step = M.fromListWith (+) . concatMap replace . M.assocs

    replace (x@(a,b),n)
      | c <- rules A.! x, c /= '\0' = [ ((a,c),n), ((c,b),n) ]
      | otherwise                   = [ (x,n) ]

    summarize m = maximum xs - minimum xs
      where
        xs = M.elems . M.adjust (1+) (head base) . M.mapKeysWith (+) snd $ m

part1 (base,rules) = polymerize base rules 10

part2 (base,rules) = polymerize base rules 40
