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
  do inp <- getInputLines parse 12
     --mapM_ print $ M.toList (part1 inp
     let asgiven  = map (\[a,b] -> (a,[b])) inp
         opposite = map (\[a,b] -> (b,[a])) inp
         fixend = map \case ("end",_) -> ("end",[]); x -> x
         links = M.fromListWith (++) (fixend (asgiven ++ opposite))
     mapM_ print (M.toList links)
     let paths = part1 links
     print (length paths)
     mapM_ print (part2 links)
  where
    parse = words . map sep
      where
        sep '-' = ' '
        sep  x  =  x

part1 links = map reverse toend
  where
    toend = filter (\(x:_) -> x == "end") $ filter (not . null) paths

    paths = map (\(_,_,path) -> path) $ bfsOn repr next [start]

    start = ("start",S.singleton "start",[])

    next ("end",s,("end":_)) = []
    next ("end",s,path) = [ ("end",s,"end":path) ]
    next (here,seenSmall,path) =
      concatMap pick (links M.! here)
      where
        pick there@(t:_)
          | isLower t && there `S.notMember` seenSmall =
            [ (there,S.insert there seenSmall,here:path) ]
          | isUpper t =
            [ (there,seenSmall,here:path) ]
          | otherwise =
            []

    repr (here,seen,path) = (here,seen,path)

part2 links = (:[]) . length $ map reverse toend -- length $ filter rule $ map (filter small) $ map reverse toend
  where

    rule xs
      | Just xs <- counts M.!? 2 = length xs == 1
      | otherwise = False
      where
        counts = M.fromListWith (++) [ (1,[x]) | x <- xs ]

    small "start" = False
    small "end" = False
    small (x:_) = isLower x

    toend = filter (\(x:_) -> x == "end") $ filter (not . null) paths

    paths = map (\(_,_,path) -> path) $ bfsOn repr next [start]

    start = ("start",M.singleton "start" (-1),[])

    next ("end",s,("end":_)) = []

    next ("end",s,path) = [ ("end",s,"end":path) ]

    next (here,seenSmall,path) =
      concatMap pick (links M.! here L.\\ ["start"])

      where
        pick there
          | 2 `elem` M.elems seenSmall = picksingles there
          | otherwise                  = allowonedouble there

        picksingles there@(t:_)
          | isLower t && there `M.notMember` seenSmall =
            [ (there,M.insert there 1 seenSmall,here:path) ]

          | isUpper t =
            [ (there,seenSmall,here:path) ]

          | otherwise =
            []

        allowonedouble there@(t:_)
          | isLower t
          , Just times <- seenSmall M.!? there
          , times == 1 =
            [ (there,M.insert there 2 seenSmall,here:path) ]

          | isLower t =
            [ (there,M.insert there 1 seenSmall,here:path) ]

          | isUpper t =
            [ (there,seenSmall,here:path) ]

          | otherwise = []

    repr (here,seen,path) = (here,seen,path)
