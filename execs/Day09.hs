module Main (main) where

import Advent             (getInput)
import Advent.Coord       (withCoords,boundingBox,cardinal,origin,Coord(..),coordCol,coordRow)
import Advent.Search      (bfs)
import Data.Ix            (inRange)
import Data.Ord           (comparing)
import Data.Maybe         (maybeToList)
import Data.List          qualified as L
import Data.Array.Unboxed qualified as A

main =
  do inp <- getInput parse 9
     print (part1 inp)
     print (part2 inp)
  where
    parse = toArray . withCoords (read @Int . pure) . lines
    toArray :: [(Coord,Int)] -> A.Array Coord Int
    toArray ps = A.array bounds ps :: A.Array Coord Int
      where
        Just bounds = boundingBox (map fst ps)

cave `at` c
  | c `inside` cave = pure (cave A.! c)
  | otherwise       = mempty
  where
    c `inside` cave = inRange (A.bounds cave) c

cave `around` c =
  [ (c',y) | c' <- cardinal c,  y <- cave `at` c' ]

lows cave = filter low (A.assocs cave)
  where
    low (c,x) = and [ y > x | (_,y) <- cave `around` c ]

part1 cave = sum [ x + 1 | (_,x) <- lows cave ]

part2 cave = product (take 3 (L.sortBy (comparing negate) basins))
  where
    basins = map (length . basin) (lows cave)

    basin (c,x) = bfs next (c,x)
      where
        next (c,x) = [ (c',y) | (c',y) <- cave `around` c, y > x, y < 9 ]
