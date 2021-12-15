module Main (main) where

import Advent             (getInputArray)
import Advent.Coord       (Coord(..),origin,addCoord,withCoords,cardinal)
import Advent.Search      (astar)
import Data.Ix            (inRange)
import Data.Char          (digitToInt)
import Data.Maybe         (maybeToList)
import Data.List          qualified as L
import Data.Array.Unboxed qualified as A

main =
  do inp <- A.amap digitToInt <$> getInputArray 15
     print (part1 inp)
     print (part2 inp)

part1 = solve

part2 = solve . stitch

type T = A.UArray Coord Int

solve :: T -> Int
solve a = least
  where
    Just least = L.lookup end search where end = maximum (A.indices a)

    search = astar next origin

    next c = [ (d,cost,0) | d <- cardinal c, inside d, let cost = a A.! d ]

    inside = inRange (A.bounds a)

stitch :: T -> T
stitch a = A.array (origin,C (5*h - 1) (5*w - 1)) assocs
  where
    C h w = let (_,end) = A.bounds a in end `addCoord` C 1 1

    assocs = concat [ map (translate dy dx) (A.assocs a)
                    | dy <- [0..4], dx <- [0..4] ]

    translate dy dx (c,r) = (c',r')
      where
        c' = c `addCoord` C (dy * h) (dx * w)
        r' = cycle [1..9] !! (r - 1 + dx + dy)
