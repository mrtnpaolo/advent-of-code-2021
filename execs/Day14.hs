--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------
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

main =
  do inp <- f <$> getInputLines parse 14
     print (part1 inp)
     print (part2 inp)
  where
    parse = words . map sep
      where
        sep '-' = ' '
        sep '>' = ' '
        sep  x  =  x
    f ([x]:_:rs) = (x,M.fromList $ map (\case [[c1,c2],[b]] -> ((c1,c2),b)) rs)

part1 (start,rs) = score (iterate step start !! 10)
  where
    step xs = ys
      where
        pairs = zip xs (tail xs)
        ys = concatMap insert pairs ++ [ snd (last pairs) ]
        insert p@(c1,c2)
          | Just new <- rs M.!? p = [c1,new]
          | otherwise             = [c1,c2]

score xs = count (most ==) xs - count (least ==) xs
  where
    fs = L.sortOn snd (freqs xs)
    (most,_) = last fs
    (least,_) = head fs

type T = M.Map String Integer

part2 (start,rs) = score' . collapse $ iterate step start' !! 40
  where
    score' m = mostn - leastn
      where
        fs = L.sortOn snd $ M.toList m
        (most,mostn) = last fs
        (least,leastn) = head fs

    collapse m = M.fromListWith (+) singles
      where
        singles = (last start,-1) : concat
          [ [(c1,n`div`2),(c2,n`div`2)] | ([c1,c2],n) <- M.toList m ]

    rs' = M.fromList [ ([c1,c2],([c1,c],[c,c2])) | ((c1,c2),c) <- M.toList rs ]
    start' = M.fromListWith (+) $ zipWith (\a b -> ([a,b],1)) start (tail start)

    step :: T -> T
    step xs = ys
      where
        ys = M.fromListWith (+) (concatMap f (M.toList xs))

        f ((c1c2),n)
          | Just (new1,new2) <- rs' M.!? c1c2 = [(new1,n),(new2,n)]
          | otherwise                         = [(c1c2,n)]
