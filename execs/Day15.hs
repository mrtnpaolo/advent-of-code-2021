--------------------------------------------------------------------------------
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
  do inp <- getInput parse 15
     mapM_ putStrLn (part1 inp)
     print (part2 inp)
  where
    parse = M.fromList . withCoords digitToInt . lines

-- 554 wrong
-- 382 wrong
-- 384 wrong
-- 386 should be it, I was removing the origin twice, which has value 2

part1 m = take 3 $ map (show . risk) (L.sortOn risk sols)
  where
    s (path,(c,r)) =
      L.intercalate "\n"
        [ showCoords path
        , "(" ++ show c ++ ") risk=" ++ show r ]

    sols = filter complete search

    risk (path,(c,r)) = r

    complete (path,(c,r)) = c == end

    search = bfsOn repr next [([origin],(origin,0))]

    repr (path,(c,r)) = (c,r)

    Just (_,end) = boundingBox (M.keys m)

    next (path,(c,_)) | c == end = []
    next (path,(c,r)) =
      [ (path',(d,r'))
      | d <- cardinal c
      , rd <- maybeToList (m M.!? d)
      , let path' = d:path
      , let r'    = r + rd
      , r' < 400
      ]

part2 _ = ()
