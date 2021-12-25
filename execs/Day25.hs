module Main (main) where

import Advent.Input       (getInputArray)
import Advent.Coord       (Coord(..),right,below)
import Data.Array.Unboxed qualified as A

main =
  do inp <- getInputArray 25
     print (part1 inp)

type A = A.UArray Coord Char

-- p = drawCoords . M.fromList . A.assocs :: A -> String

tick :: A -> A
tick a = a2
  where
    (_,C yM xM) = A.bounds a

    es = [ [(c,'.'),(c','>')]
         | (c@(C y x),'>') <- A.assocs a
         , let c' | x == xM   = C y 0
                  | otherwise = right c
         , a A.! c' == '.' ]
    a1 = a A.// concat es

    ss = [ [(c,'.'),(c','v')]
         | (c@(C y x),'v') <- A.assocs a1
         , let c' | y == yM   = C 0 x
                  | otherwise = below c
         , a1 A.! c' == '.' ]
    a2 = a1 A.// concat ss

part1 = solve 1 . iterate tick

solve n (x:y:xs)
  | x==y      = n
  | otherwise = solve (n+1) (y:xs)
