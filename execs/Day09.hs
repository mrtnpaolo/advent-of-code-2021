module Main (main) where

import Advent                   (getInput)
import Advent.Coord             (withCoords,cardinal)
import Advent.Search            (bfs)
import Data.Ord                 (comparing)
import Data.Maybe               (maybeToList)
import Data.List qualified as L (lookup,sortBy)

main =
  do inp <- getInput parse 9
     print (part1 inp)
     print (part2 inp)
  where
    parse = withCoords (read @Int . pure) . lines

cave `around` c =
  [ (c',y) | c' <- cardinal c, y <- maybeToList (L.lookup c' cave) ]

lows cave = filter low cave
  where
    low (c,x) = and [ y > x | (c',y) <- cave `around` c ]

part1 cave = sum [ x + 1 | (_,x) <- lows cave ]

part2 cave = product (take 3 (L.sortBy (comparing negate) basins))
  where
    basins = map (length . basin) (lows cave)

    basin (c,x) = bfs next (c,x)
      where
        next (c,x) = [ (c',y) | (c',y) <- cave `around` c, y > x, y < 9 ]
