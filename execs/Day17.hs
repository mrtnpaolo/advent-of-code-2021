module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInput parse 17
     print (part1 inp)
     print (part2 inp)
  where
    parse = target . words . clean
    clean = map \case c | c `elem` "xy-" || isDigit c -> c | otherwise -> ' '
    target ["x",xm,xM,"y",ym,yM] =
      (C (read ym) (read xm),C (read yM) (read xM))

part1 (C (abs -> ym) _,_) = (ym * (ym - 1)) `div` 2

tick (p@(C py px),v@(C vy vx)) = (C py' px',C vy' vx')
  where
    py' = py + vy
    px' = px + vx

    vy' = vy - 1
    vx' | vx > 0  = vx - 1
        | vx < 0  = vx + 1
        | vx == 0 = 0

shoot area v = go (C 0 0,v)
  where
    go q@(p@(C y x),(C yv xv))
      | y < ym    = []
      | otherwise = p : go (tick q)
    (C ym _,_) = area

part2 area = count (any inside) [ shoot area v
                                | v <- range (C (-180) 1,C 200 140) ]
  where
    inside (C y x) = ym <= y && y <= yM && xm <= x && x <= xM
    (C ym xm,C yM xM) = area
