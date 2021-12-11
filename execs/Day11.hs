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

step m = (flashes,normalize final)
  where
    m'              = M.map succ m
    flashing        = M.keysSet (M.filter (> 9) m')
    (flashes,final) = extend S.empty flashing m'

extend seen flashing m
  | S.null flashing = (S.size seen,m)
  | otherwise       = extend seen' flashing' m'
    where
      seen'     = S.union seen flashing
      m'        = L.foldl' increaseAdjacents m (S.toList flashing)
      flashing' = S.difference (M.keysSet (M.filter (> 9) m')) seen'

increaseAdjacents m c = L.foldl' (\m c -> M.adjust succ c m) m (neighbors c)

part1 m = sum (take 100 flashes)
  where
    flashes = L.unfoldr (Just . step) m

part2 m = i
  where
    flashes = L.unfoldr (Just . step) m
    Just i  = L.elemIndex (M.size m) (0 : flashes)
