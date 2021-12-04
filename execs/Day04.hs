module Main (main) where

import Advent

import Data.Ix (range)
import Data.List (inits,scanl',foldl',foldl1')
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A

type Board = A.UArray (Int,Int) Int

main =
  do inp <- getInput parse 4
     print (part1 inp)
     print (part2 inp)
  where
    parse xs = (ns,boards)
      where
        ns = map (read @Int) . words . map sep . head . lines $ xs
        sep ',' = ' '
        sep  x  =  x

        boards :: [Board]
        boards = collect [] . tail . tail . lines $ xs

        collect bs [] = bs
        collect bs xs = collect (bs ++ [b]) xs'
          where
            b = toArr (take 5 xs)
            xs' = drop 6 xs
            toArr :: [String] -> Board
            toArr xs = A.listArray ((0,0),(4,4)) entries
              where
                entries = concatMap (map (read @Int) . words) xs

winning :: [Int] -> Board -> Bool
winning drawn board =
  or [ all (`IS.member` drawn') [ board A.! c | c <- cs ]
     | cs <- rows ++ cols ]
  where
    rows = [ [ (i,j) | j <- [0..4] ] | i <- [0..4] ]
    cols = [ [ (i,j) | i <- [0..4] ] | j <- [0..4] ]
    drawn' = IS.fromList drawn

part1 (ns,bs) = turns
  where
    draws = inits ns
    turns = zipWith check draws (repeat bs)
      where
        check ns bs = [ (i,score ns b) | (i,b) <- zip [0..] bs, winning ns b ]

score ns b = last ns * sum
  [ n
  | c <- range ((0,0),(4,4))
  , let n = b A.! c
  , n `notElem` ns ]

part2 (ns,bs) = winners
  where
    turns = part1 (ns,bs)

    b = length bs

    (winners,_) = foldl' track ([],IS.empty) turns
      where
        track (winners,seen) [] = (winners,seen)
        track (winners,seen) xs = foldl' collect (winners,seen) xs
          where
            collect (winners,seen) (i,s)
              | i `IS.member` seen = (winners,seen)
              | IS.size seen == b = (winners,seen)
              | otherwise = (winners ++ [(i,s)],IS.insert i seen)
