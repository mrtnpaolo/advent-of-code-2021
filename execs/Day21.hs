module Main (main) where

import Advent.Input    (getInputLines)
import Data.Map.Strict qualified as M

main =
  do inp <- getInputLines parse 21
     print (part1 inp)
     print (part2 inp)

parse = read @Integer . last . words

part1 [p1,p2] = go 0 (cycle [1..100]) p1 0 p2 0
  where
    go turn _    p1 s1 p2 s2 | s1 >= 1000 = s2 * 3 * turn
    go turn _    p1 s1 p2 s2 | s2 >= 1000 = s1 * 3 * turn
    go turn dice p1 s1 p2 s2
      | even turn = go (turn+1) dice' p1' s1' p2  s2
      | odd  turn = go (turn+1) dice' p1  s1  p2' s2'
      where
        (d1:d2:d3:dice') = dice
        p1' = forward (d1+d2+d3) p1; s1' = s1 + p1'
        p2' = forward (d1+d2+d3) p2; s2' = s2 + p2'

forward n p = ((p - 1 + n) `mod` 10) + 1

type GM = M.Map (Integer,Integer,Integer,Integer) Integer

part2 [p1,p2] = go True 0 0 (M.singleton (p1,0,p2,0) 1 :: GM)
  where
    go turn p1w p2w m
      | M.null m2 = max p1w' p2w'
      | otherwise = go (not turn) p1w' p2w' m3
      where
        (p1wins,m1) = M.partitionWithKey p1winning m
        (p2wins,m2) = M.partitionWithKey p2winning m1

        p1w' = p1w + sum p1wins
        p2w' = p2w + sum p2wins

        m3 = M.fromListWith (+)
          [ (s',n*n')
          | (s ,n ) <- M.toList m2
          , (s',n') <- M.toList (quantumplay turn s) ]

    p1winning (_,s1,_,_ ) _ = s1 >= 21
    p2winning (_,_ ,_,s2) _ = s2 >= 21

quantumoutcomes = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]

quantumplay turn (p1,s1,p2,s2) = outcomes
  where
    outcomes = M.fromListWith (+)
      [ (apply roll,mult) | (roll,mult) <- quantumoutcomes ]

    apply roll
      | turn      = (p1',s1',p2 ,s2 )
      | otherwise = (p1 ,s1 ,p2',s2')
      where
        p1' = forward roll p1; s1' = s1 + p1'
        p2' = forward roll p2; s2' = s2 + p2'
