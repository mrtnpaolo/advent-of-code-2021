module Main (main) where

import Advent          (getInputLines,count,dfs,dfsOn)
import Data.Char       (isLower,isUpper)
import Data.Map.Strict qualified as M

main =
  do links <- toLinks <$> getInputLines parse 12
     mapM_ print (M.toList links)
     let paths = part1 links
     --mapM_ print paths
     print (length paths)
     let paths2 = part2 links
     --mapM_ print paths2
     print (length paths2)
  where
    parse = words . map \case '-' -> ' '; x -> x
    toLinks inp = M.fromListWith (++) (fixends (asgiven ++ opposite))
      where
        asgiven  = map (\[a,b] -> (a,[b])) inp
        opposite = map (\[a,b] -> (b,[a])) inp
        fixends  = map \case ("end",_) -> ("end",[]); x -> x
                 . map \(from,to) -> (from,filter ("start" /=) to)

explore p links = paths
  where
    paths = filter complete search

    complete ("end":_) = True
    complete _         = False

    search = dfs next ["start"]

    next path@(here:_) = concatMap allowed (links M.! here)
      where
        allowed there@(t:_)
          | isUpper t    = [ there:path ]
          | p path there = [ there:path ]
          | otherwise    = []

part1 = explore pick

pick path there = there `notElem` path

part2 = explore pick'

pick' path there
  | or [ count (x ==) path == 2 | x <- filter (isLower . head) path ]
  = pick path there
  | otherwise
  = pick2 path there

pick2 path there@(t:_) = count (there ==) path <= 1
