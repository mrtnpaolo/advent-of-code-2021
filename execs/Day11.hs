module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace
import Text.Printf

showMap = showCoordsWith 1 (\n -> printf "%1d" n) . M.toList

main =
  do inp <- getInput parse 11
     putStrLn (showMap inp)
     mapM_ (\(tot,m) -> printf "%s\n%d\n\n" (showMap m) tot) (part1 inp)
     print (part2 inp)
  where
    parse :: String -> M.Map Coord Int
    parse = M.fromList . withCoords f . lines
      where
        f = read @Int . pure

part1 m = take 1 . drop 99 $ (tail $ iterate step (0,m))

step (total,m) = (total + n',normalize final)

  where

    m' = M.map succ m

    flashing = M.keysSet (M.filter (> 9) m')

    n = S.size flashing

    (n',final) = extend n S.empty flashing m'

    normalize = M.map (\x -> if x > 9 then 0 else x)

extend flashes alreadyFlashed flashing m

  | S.null flashing = (flashes,m)

  | otherwise = extend flashes' alreadyFlashed' newlyFlashing m'

    where

      flashes' = flashes + S.size newlyFlashing

      alreadyFlashed' = S.union alreadyFlashed flashing

      (newlyFlashing,m') = L.foldl increaseAdjacents (S.empty,m) (S.toList flashing)

      increaseAdjacents acc c = L.foldl' f acc candidates
        where
          candidates = filter (`M.member` m) (neighbors c)

          f (new,m) c
            | c `S.notMember` alreadyFlashed' &&
              m' M.! c > 9 = (S.insert c new,m')
            | otherwise    = (new           ,m')
            where
              m' = M.insertWith (+) c 1 m

part2 m = 1 + i
  where
    flashes = [ tot | (tot,_) <- iterate step (0,m) ]
    increments = zipWith (-) (tail flashes) flashes
    Just i = L.findIndex (M.size m ==) increments

