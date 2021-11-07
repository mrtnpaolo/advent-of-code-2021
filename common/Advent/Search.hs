module Advent.Search
  ( dfs, dfsOn
  , bfs, bfsOn
  , astar, astarOn
  ) where

import Advent.Deque  qualified as D
import Advent.PQueue qualified as PQ

import Data.Set    qualified as S
import Data.IntSet qualified as IS

import Data.Foldable (foldl')

{-# INLINE dfs #-}
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs next start = dfsOn id next start

{-# INLINE[0] dfsOn #-}
dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn repr next start = loop S.empty [start]
  where
    loop _ [] = []
    loop seen (x:xs)
      | r `S.member` seen = loop seen xs
      | otherwise         = x : loop seen' (next x ++ xs)
      where
        r = repr x
        seen' = S.insert r seen

{-# RULES "dfsOn/Int" dfsOn = dfsOnInt #-}
{-# INLINE dfsOnInt #-}
dfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
dfsOnInt repr next start = loop IS.empty [start]
  where
    loop _ [] = []
    loop seen (x:xs)
      | r `IS.member` seen = loop seen xs
      | otherwise          = x : loop seen' (next x ++ xs)
      where
        r = repr x
        seen' = IS.insert r seen

{-# INLINE bfs #-}
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs next start = bfsOn id next [start]

{-# INLINE[0] bfsOn #-}
bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOn repr next starts = loop S.empty (D.fromList starts)
  where
    loop _ D.Empty = []
    loop seen (x D.:<| xs)
      | r `S.member` seen = loop seen xs
      | otherwise         = x : loop seen' (D.appendList nexts xs)
      where
        r = repr x
        seen' = S.insert r seen
        nexts = next x

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> [a] -> [a]
bfsOnInt repr next starts = loop IS.empty (D.fromList starts)
  where
    loop _ D.Empty = []
    loop seen (x D.:<| xs)
      | r `IS.member` seen = loop seen xs
      | otherwise          = x : loop seen' (D.appendList nexts xs)
      where
        r = repr x
        seen' = IS.insert r seen
        nexts = next x

{-# INLINE astar #-}
astar :: Ord a => (a -> [(a,Int,Int)]) -> a -> [(a,Int)]
astar next start = astarOn id next start

{-# INLINE astarOn #-}
astarOn :: Ord r => (a -> r) -> (a -> [(a,Int,Int)]) -> a -> [(a,Int)]
astarOn repr next start = loop S.empty (PQ.singleton 0 (0,start))
  where
    loop _    PQ.Empty               = []
    loop seen ((cost,x) PQ.:<| rest)
      | r `S.member` seen = loop seen rest
      | otherwise         = (x,cost) : loop seen' rest'
      where
        r = repr x
        seen' = S.insert r seen
        rest' = foldl' (\q (p,v) -> PQ.insert p v q) rest nexts
        nexts = [ (cost + y_cost + y_dist,(cost + y_cost,y)) | (y,y_cost,y_dist) <- next x ]
