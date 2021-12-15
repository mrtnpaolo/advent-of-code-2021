module Main (main) where

import Advent          (getInput)
import Advent.Coord    (Coord(..),origin,addCoord,withCoords,cardinal)
import Advent.Search   (astar)
import Data.Char       (digitToInt)
import Data.Maybe      (maybeToList)
import Data.List       qualified as L
import Data.Map.Strict qualified as M

main =
  do inp <- getInput parse 15
     print (part1 inp)
     print (part2 inp)
  where
    parse = M.fromList . withCoords digitToInt . lines

part1 = solve

part2 = solve . stitch

solve m = least
  where
    Just least = L.lookup end search where end = maximum (M.keys m)

    search = astar next origin

    next c = [ (d,cost,0) | d <- cardinal c, cost <- maybeToList (m M.!? d) ]

stitch m = M.unions [ translate dy dx m | dy <- [0..4], dx <- [0..4] ]

translate dy dx m = M.map risk (M.mapKeys move m)
  where
    C h w = maximum (M.keys m) `addCoord` C 1 1

    move (C y x) = C (y + dy*h) (x + dx*w)

    risk r = cycle [1..9] !! ((r - 1) + dx + dy)
