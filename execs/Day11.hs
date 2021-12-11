module Main (main) where

import Advent          (getInput)
import Advent.Coord    (withCoords,neighbors)
import Data.Char       (digitToInt)

import Data.List       qualified as L
import Data.Set        qualified as S
import Data.Map.Strict qualified as M

main =
  do inp <- getInput parse 11
     print (part1 inp)
     print (part2 inp)
  where
    parse = M.fromList . withCoords digitToInt . lines

normalize = M.map (\x -> if x > 9 then 0 else x)

step m = Just (flashes,normalize final)
  where
    m' = M.map succ m
    flashing = M.keysSet (M.filter (> 9) m')
    (flashes,final) = extend S.empty flashing (S.size flashing) m'

extend seen flashing flashes m
  | S.null flashing = (flashes,m)
  | otherwise       = extend seen' flashing' flashes' m'
    where
      seen'     = S.union seen flashing
      m'        = L.foldl' increaseAdjacents m (S.toList flashing)
      flashing' = S.difference (M.keysSet (M.filter (> 9) m')) seen'
      flashes'  = flashes + S.size flashing'

increaseAdjacents m c =
  L.foldl' (\m c -> M.insertWith (+) c 1 m) m (filter (`M.member` m) (neighbors c))

part1 m = sum (take 100 flashes)
  where
    flashes = L.unfoldr step m

part2 m = i
  where
    flashes = L.unfoldr step m
    Just i  = L.elemIndex (M.size m) (0 : flashes)
