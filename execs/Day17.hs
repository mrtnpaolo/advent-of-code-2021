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
     --print (part1 inp)
     print (part2 inp)
  where
    parse = f . words . map \case c | c `elem` "xy-" -> c; c | not (isDigit c) -> ' '; c -> c
    f ["x",xm,xM,"y",ym,yM] = (C (read ym) (read xm),C (read yM) (read xM))

tick (p@(C py px),v@(C vy vx)) = (C py' px',C vy' vx')
  where
    py' = py + vy
    px' = px + vx

    vy' = vy - 1
    vx' | vx > 0 = vx - 1
        | vx < 0 = vx + 1
        | vx == 0 = 0

shoot area v = takeWhile reasonable $ iterate tick start
  where
    start = (C 0 0,v)

    reasonable (C y _,_) = y >= ym
    ym = minimum [ y | C y _ <- S.toList area ]

part1 corners = maximum
  [ maxY path
  | v <- range (C (-500) (-500),C 1000 1000)
  , let path = shoot area v
  , any (`S.member` area) (map fst path)
  ]
  where
    area = S.fromList (range corners)

part2 corners = count (any (`S.member` area) . map fst)
  [ shoot area v
  | v <- range (C (-500) (-500),C 1000 1000)
  ]
  where
    area = S.fromList (range corners)

maxY path = maximum [ y | (C y _,v) <- path ]
