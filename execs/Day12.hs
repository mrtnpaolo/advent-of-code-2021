module Main (main) where

import Advent          (getInputLines,count,bfs)
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

explore pick links = paths
  where
    paths = filter complete (bfs next ["start"])

    complete ("end":_) = True
    complete _         = False

    next path@(here:_) = concatMap (pick links path) (links M.! here)

part1 = explore pick

pick links path there@(t:_)
  | isLower t && there `notElem` path = [there : path]
  | isUpper t                         = [there : path]
  | otherwise                         = []

part2 = explore pick'

pick' links path there
  | or [ count (x ==) path == 2 | x <- filter (isLower . head) path ]
  = pick links path there
  | otherwise
  = pick2 links path there

pick2 links path there@(t:_)
  | isLower t && count (there ==) path <= 1 = [there : path]
  | isUpper t = [there : path]
  | otherwise = []
