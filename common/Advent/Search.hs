module Advent.Search
  ( dfs
  , dfsOn
  ) where

import Advent.Deque

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
