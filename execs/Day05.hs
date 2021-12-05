module Main (main) where

import Advent                         (getInputLines,count)
import Advent.Coord                   (Coord(..),addCoord)
import Data.List                      (partition,foldl')
import Data.Map.Strict qualified as M (empty,insertWith,elems)

orthogonal (C y x,C y' x') = y == y' || x == x'

line (from,to) = ray from
  where
    ray c | c == to = [to] | otherwise = c : ray (c `addCoord` step)

    step = unit (to `addCoord` neg from)

    unit (C y x) = C (signum y) (signum x)
    neg  (C y x) = C (-y) (-x)

main =
  do inp <- getInputLines parse 5
     let (ort,dia) = partition orthogonal inp
     print (part1 ort)
     print (part2 (ort ++ dia))
  where
    parse xs = (C y x,C y' x')
      where
        sep x | x `elem` ",->" = ' ' | otherwise = x
        [x,y,x',y'] = map (read @Int) . words . map sep $ xs

part1 pairs = count (>= 2) (M.elems m)
  where
    m = foldl' (\m c -> M.insertWith (+) c 1 m) M.empty (concatMap line pairs)

part2 = part1
