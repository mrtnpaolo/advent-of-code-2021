module Main (main) where

import Advent       (getInputLines)
import Advent.Coord (Coord(..),origin,north,east,south,addCoord)
import Data.List    (foldl')

data Movement = Aim Int | Move Int
  deriving (Show)

main =
  do inp <- getInputLines words 2
     print (part1 (concatMap toCoord inp))
     print (part2 (map toMove inp))
  where
    toCoord ["forward",read -> n] = replicate n east
    toCoord ["up"     ,read -> n] = replicate n north
    toCoord ["down"   ,read -> n] = replicate n south

    toMove  ["forward",read -> n] = Move n
    toMove  ["up"     ,read -> n] = Aim (negate n)
    toMove  ["down"   ,read -> n] = Aim n

part1 xs = endy*endx
  where
    C endy endx = foldl' addCoord origin xs

part2 xs = endy*endx
  where
    (C endy endx,_) = foldl' step (origin,0) xs

    step (c    ,aim) (Aim  m) = (c      ,aim+m)

    step (C y x,aim) (Move n) = (C y' x',aim  )
      where
        y' = y + n*aim
        x' = x + n
