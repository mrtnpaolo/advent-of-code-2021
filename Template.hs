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
  do inp <- getInputLines parse _
     print (part1 inp)
     print (part2 inp)
  where
    parse = words . map \case c | c `elem` ",->" -> ' '; c -> c

part1 = id

part2 = const ()
