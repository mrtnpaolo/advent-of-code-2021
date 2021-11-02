module Advent.Search
  ( dfs, dfsOn
  , bfs, bfsOn
  ) where

import Advent.Deque (Deque(Empty,(:<|)))
import Advent.Deque qualified as D

import Data.Set    qualified as S
import Data.IntSet qualified as IS

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
    loop _ Empty = []
    loop seen (x :<| xs)
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
    loop _ Empty = []
    loop seen (x :<| xs)
      | r `IS.member` seen = loop seen xs
      | otherwise          = x : loop seen' (D.appendList nexts xs)
      where
        r = repr x
        seen' = IS.insert r seen
        nexts = next x
